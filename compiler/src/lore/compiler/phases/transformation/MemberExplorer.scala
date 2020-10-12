package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.VirtualMember
import lore.compiler.types._

import scala.collection.immutable.HashMap

/**
  * For any given type, returns a list of members that can be accessed through the type. This is trivial for
  * struct types and traits, but quickly becomes interesting when intersection types are considered.
  */
object MemberExplorer {

  // TODO: Maybe just turn members into a lazy property of types? I'm worried about race conditions here...
  //       That'd mean we'd have to intern types, though, and just moves the problem a goalpost further.
  //       Other idea: Give members a mutex, keep the cache, but also give each member map to the respective
  //       type.
  type MemberMap = Map[String, VirtualMember]
  private var cache = HashMap.empty[Type, MemberMap]

  /**
    * Finds a virtual member with the given name within the given type.
    */
  def find(name: String, tpe: Type)(implicit position: Position): Compilation[VirtualMember] = {
    members(tpe).flatMap { members =>
      members.get(name) match {
        case None => Compilation.fail(MemberNotFound(name, tpe, position))
        case Some(member) => member.compiled
      }
    }
  }

  /**
    * Return all virtual members associated with this type. Any ambiguity errors are attached the given position,
    * which should be the code location where the member is accessed. The result of this function is cached so
    * that we don't have to recompute the member map the next time it is accessed.
    */
  def members(tpe: Type)(implicit position: Position): Compilation[MemberMap] = {
    cache.get(tpe) match {
      case None =>
        membersOf(tpe).map { map =>
          cache = cache + (tpe -> map)
          map
        }
      case Some(map) => map.compiled
    }
  }

  private def membersOf(tpe: Type)(implicit position: Position): Compilation[MemberMap] = {
    def component(underlying: DeclaredType): (String, VirtualMember) = {
      underlying.name -> VirtualMember(underlying.name, underlying, isComponent = true)
    }

    tpe match {
      case structType: StructType =>
        // A struct type has exactly the members that are declared in it. One might think that it could have additional
        // members by the way of traits extending component types, but all such components are already guaranteed to
        // be part of the struct. The same is true for owned-by types of such components. The struct has to adhere to
        // each component's owned-by type anyway, so there won't be any additional members defined through such an
        // owned-by constellation.
        HashMap(structType.definition.members.map(m => m.name -> m.asVirtualMember): _*).compiled

      case traitType: TraitType =>
        // A trait's members are limited to the components that the trait contains. The same stipulation about owned-by
        // types not inducing any additional members (see the struct explanation above) also holds for traits. The trait
        // itself has to satisfy the owned-by typing of each of its components, which means that no additional members
        // can be added through an owned-by type to the trait's list of members.
        HashMap(traitType.inheritedComponentTypes.map(c => component(c.underlying)).toVector: _*).compiled

      case ComponentType(underlying) =>
        // A component type directly defines a single member that has the name and type of its underlying declared
        // type. It also additionally defines members based on its "owned by" type.
        MemberExplorer.members(underlying.ownedBy).map { ownedByMembers =>
          ownedByMembers + component(underlying)
        }

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
        // two 'destroy' functions being declared in two separate interfaces). For now, it is still the simplest
        // solution and hopefully a sufficient one.
        val allMembers = types.toVector.map(MemberExplorer.members).simultaneous.map(_.flatMap(_.values))
        allMembers.flatMap { members =>
          members.groupBy(_.name).map {
            case (_, Vector(member)) => (member.name -> member).compiled
            case (name, members) if members.length >= 2 =>
              if (members.exists(_.isMutable)) {
                // If we have multiple members and at least one member is mutable, ALL members must be mutable
                // and share the exact same type.
                if (members.exists(_.isImmutable)) {
                  Compilation.fail(MutabilityAmbiguity(name, tpe, position))
                } else {
                  val typesAreEqual = members.sliding(2).forall { case Vector(a, b) => a.tpe == b.tpe }
                  if (!typesAreEqual) {
                    Compilation.fail(MutableTypeAmbiguity(name, tpe, position))
                  } else {
                    (name -> members.head).compiled
                  }
                }
              } else {
                // If we have multiple immutable members with different types, their types must be compatible in some
                // way. We must look for a member whose type is a supertype of all other member types. If no such type
                // exists, we have a true ambiguity.
                members.find(m1 => members.forall(m2 => m2.tpe <= m1.tpe)) match {
                  case None => Compilation.fail(TypeAmbiguity(name, tpe, position))
                  case Some(member) => (name -> member).compiled
                }
              }
          }.toVector.simultaneous.map(HashMap(_: _*))
        }

      case _ => (HashMap.empty: MemberMap).compiled
    }
  }

  case class MutabilityAmbiguity(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a member of a type $tpe. This type has a mutable member ambiguity" +
      s" in the member $name, as there are at least two individual members with the same name but differing mutability."
  }

  case class MutableTypeAmbiguity(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a mutable member of a type $tpe. This type has a member ambiguity" +
      s" in the member $name, as there are at least two individual members with the same name but with differing types."
  }

  case class TypeAmbiguity(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a member of a type $tpe. This type has a member ambiguity" +
      s" in the member $name, as there are at least two individual members with the same name having incompatible types."
  }

  case class MemberNotFound(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"A member $name does not exist within the type $tpe."
  }

}
