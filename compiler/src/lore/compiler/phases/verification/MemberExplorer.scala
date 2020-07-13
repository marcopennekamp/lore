package lore.compiler.phases.verification

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.core.Compilation.C
import lore.compiler.feedback._
import lore.compiler.types._

/**
  * For any given type, returns a list of members that can be accessed through the type. This is trivial for
  * class types, but quickly becomes interesting when intersection types are considered.
  */
object MemberExplorer {
  // TODO: Implement the default naming scheme described in the specification.
  // TODO: Implement some form of caching! We can cache HARD here. Like, it's really not effective to calculate
  //       the full member list every time just to access a single member.
  // TODO: Unit-test this class.

  // TODO: We need to consider ownership restrictions here! If a component is owned by any type A, we must consider
  //       members of that type A. (PRIORITY!)

  /**
    * Finds a virtual member with the given name within the given type.
    */
  def find(name: String, tpe: Type, position: Position): C[VirtualMember] = {
    members(tpe, position).flatMap { members =>
      members.get(name) match {
        case None => Compilation.fail(MemberNotFound(name, tpe, position))
        case Some(member) => Compilation.succeed(member)
      }
    }
  }

  /**
    * Return all virtual members associated with this type. Any ambiguity errors are attached the given position,
    * which should be the point in the code where the member is accessed.
    */
  def members(tpe: Type, position: Position): C[Map[String, VirtualMember]] = {
    memberList(tpe, position).map { list => list.map(vm => (vm.name, vm)).toMap }
  }

  private def memberList(tpe: Type, position: Position): C[List[VirtualMember]] = {
    tpe match {
      // Base cases (no recursion).
      case AnyType | _: BasicType | _: LabelType =>
        Compilation.succeed(Nil)
      case ComponentType(underlying) =>
        // A component type defines a single member that has the name and type of the given underlying class.
        Compilation.succeed(VirtualMember(underlying.definition.name, underlying, isComponent = true) :: Nil)
      case ListType(_) | MapType(_, _) =>
        // List and map types have a baked-in virtual member for size.
        Compilation.succeed(VirtualMember("size", BasicType.Int) :: Nil)
      case ProductType(components) =>
        // A tuple does not inherit any of its components' members. Rather, it has a named member for each component.
        Compilation.succeed {
          components.zipWithIndex.map { case (tpe, index) => VirtualMember(s"_$index", tpe, underlying = None) }
        }
      case classType: ClassType =>
        // A class type obviously has its own members.
        Compilation.succeed(classType.definition.members.map(_.asVirtualMember))

      // Complex cases (recursion).
      case SumType(_) =>
        // TODO: We can technically access a virtual member if its name and type are found in all of the sum type's
        //       types. This would be a kind of very powerful structural typing. Since it's questionable whether such
        //       a feature could be useful, we will pretend that the sum type has no members for now.
        // Note that we can't have a notion of "left" and "right" since sum types are commutative.
        Compilation.succeed(Nil)
      case IntersectionType(types) =>
        // In the case of an intersection type, if a virtual member is part of one of the types, it is also part
        // of the intersection type. This leads to a possible problem with ambiguities: if two or more types define
        // a virtual member of the same name, the definition is technically ambiguous.
        // We can resolve this by requiring that members of the same name must also have the same type. This throws
        // up another problem: Members become part of a global namespace. This is similar to an issue in languages
        // like Java where functions defined in an interface are part of an implicit global namespace (just imagine
        // two 'destroy' functions being declared in two separate interfaces).
        // TODO: Consider if we can solve the "global namespace" problem in a better way.
        val allMembers = types.toList.map { tpe => MemberExplorer.memberList(tpe, position) }.simultaneous.map(_.flatten)
        allMembers.flatMap { members =>
          // Let's look at the virtual members of the type grouped by their names.
          members.groupBy(_.name).map {
            case (_, List(member)) =>
              // A single member is always valid!
              Compilation.succeed(member)
            case (name, members) if members.length >= 2 =>
              // If we have multiple members with different types, their types must be compatible in some way. That is
              // we must look for a member whose type is a supertype of all other member types. If no such type exists,
              // we have a true ambiguity.
              // TODO: If this ever leads to performance problems, consider a smarter algorithm.
              members.find(m1 => members.forall(m2 => m2.tpe <= m1.tpe)) match {
                case None => Compilation.fail(AmbiguousTypeMember(name, tpe, position))
                case Some(member) => Compilation.succeed(member)
              }
            case _ => throw CompilationException("This case should never be reached.")
          }.toList.simultaneous
        }
    }
  }

  case class AmbiguousTypeMember(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"You are trying to access a member of a type $tpe. This type has a member ambiguity" +
      s" in the member $name, as there are at least two individual members of the same name with incompatible types. Please" +
      s" fix the ambiguity before you try to access a member of that type."
  }

  case class MemberNotFound(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"A member $name does not exist within the type $tpe."
  }
}
