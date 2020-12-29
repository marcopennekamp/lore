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

  // TODO: Maybe just turn members into a lazy property of types? I'm worried about cache race conditions here...
  //       That'd mean we'd have to intern types, though, and just moves the problem a goalpost further.
  //       Other idea: Give members a mutex, keep the cache, but also give each member map to the respective
  //       type. That way, a type only has to ask once for its cached member map.
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
    tpe match {
      case structType: StructType =>
        HashMap(structType.definition.properties.map(property => property.name -> property.asVirtualMember): _*).compiled

      case shapeType: ShapeType =>
        shapeType.properties.map { case (name, property) => name -> property.asVirtualMember }.compiled

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
