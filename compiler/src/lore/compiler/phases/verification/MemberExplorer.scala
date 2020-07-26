package lore.compiler.phases.verification

import lore.compiler.core
import lore.compiler.core.{Compilation, CompilationException, Position, Error}
import lore.compiler.core.Compilation.C
import lore.compiler.types._

import scala.collection.immutable.HashMap

/**
  * For any given type, returns a list of members that can be accessed through the type. This is trivial for
  * class types, but quickly becomes interesting when intersection types are considered.
  */
object MemberExplorer {
  type MemberMap = Map[String, VirtualMember]
  private var cache = HashMap.empty[Type, MemberMap]

  /**
    * Finds a virtual member with the given name within the given type.
    */
  def find(name: String, tpe: Type)(implicit position: Position): C[VirtualMember] = {
    members(tpe).flatMap { members =>
      members.get(name) match {
        case None => Compilation.fail(MemberNotFound(name, tpe, position))
        case Some(member) => Compilation.succeed(member)
      }
    }
  }

  /**
    * Return all virtual members associated with this type. Any ambiguity errors are attached the given position,
    * which should be the point in the code where the member is accessed. The result of this function is cached so
    * that we don't have to recompute the member map the next time it is accessed.
    */
  def members(tpe: Type)(implicit position: Position): C[MemberMap] = {
    cache.get(tpe) match {
      case None =>
        membersOf(tpe).map { map =>
          cache = cache + (tpe -> map)
          map
        }
      case Some(map) => Compilation.succeed(map)
    }
  }

  private def membersOf(tpe: Type)(implicit position: Position): C[MemberMap] = {
    tpe match {
      // Base cases (no recursion).
      case ComponentType(underlying) =>
        // A component type directly defines a single member that has the name and type of the given underlying class.
        // It also additionally defines members based on its "owned by" type.
        underlying.ownedBy.map(MemberExplorer.members).toCompiledOption.map { ownedByMembers =>
          val name = underlying.definition.name
          ownedByMembers.getOrElse(HashMap.empty: MemberMap) + (name -> VirtualMember(name, underlying, isComponent = true))
        }
      case classType: ClassType =>
        // A class type obviously has its own members.
        Compilation.succeed(HashMap(classType.definition.members.map(m => m.name -> m.asVirtualMember): _*))

      // Complex cases (recursion).
      case tv: TypeVariable =>
        // A type variable's members are defined by its upper bound. If a member is a member of the upper bound type,
        // it must also be present in all possible instances of the type variable. If it wasn't, we would be violating
        // a basic contract of polymorphism.
        MemberExplorer.members(tv.upperBound)
      case IntersectionType(types) =>
        // In the case of an intersection type, if a virtual member is part of one of the types, it is also part
        // of the intersection type. This leads to a possible problem with ambiguities: if two or more types define
        // a virtual member of the same name, the definition is technically ambiguous.
        // We can resolve this by requiring that members of the same name must also have the same type. This throws
        // up another problem: Members become part of a global namespace. This is similar to an issue in languages
        // like Java where functions defined in an interface are part of an implicit global namespace (just imagine
        // two 'destroy' functions being declared in two separate interfaces).

        // Use the members function so that we can use caching when building member maps from complex types.
        val allMembers = types.toList.map(MemberExplorer.members).simultaneous.map(_.flatMap(_.values))
        allMembers.flatMap { members =>
          // Let's look at the virtual members of the type grouped by their names.
          members.groupBy(_.name).map {
            case (_, List(member)) =>
              // A single member is always valid!
              Compilation.succeed(member.name -> member)
            case (name, members) if members.length >= 2 =>
              // TODO: Instead of differentiating between mutable and immutable members here, we could pick our
              //       algorithm based on whether a member is accessed for a read or a write. On the other hand,
              //       this would complicate caching, and might not be consistent enough: If we can access a member
              //       to read it, why would a write be prohibited?
              if (members.exists(_.isMutable)) {
                // If we have multiple members and at least one member is mutable, we just conservatively shut down the
                // attempt altogether.
                Compilation.fail(MutableAmbiguousTypeMember(name, tpe, position))
              } else {
                // If we have multiple constant members with different types, their types must be compatible in some
                // way. That is we must look for a member whose type is a supertype of all other member types. If no
                // such type exists, we have a true ambiguity.
                members.find(m1 => members.forall(m2 => m2.tpe <= m1.tpe)) match {
                  case None => Compilation.fail(ImmutableAmbiguousTypeMember(name, tpe, position))
                  case Some(member) => Compilation.succeed(name -> member)
                }
              }
            case _ => throw CompilationException("This case should never be reached.")
          }.toList.simultaneous.map(HashMap(_: _*))
        }

      // Default case.
      case _ => Compilation.succeed(HashMap.empty)
    }
  }

  case class MutableAmbiguousTypeMember(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a member of a type $tpe. This type has a mutable member ambiguity" +
      s" in the member $name, as there are at least two individual members with the same name and at least one member is mutable."
  }

  case class ImmutableAmbiguousTypeMember(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a member of a type $tpe. This type has an immutable member ambiguity" +
      s" in the member $name, as there are at least two individual members with the same name having incompatible types."
  }

  case class MemberNotFound(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"A member $name does not exist within the type $tpe."
  }
}
