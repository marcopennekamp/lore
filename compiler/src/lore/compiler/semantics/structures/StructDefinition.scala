package lore.compiler.semantics.structures

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{ComponentType, StructType}
import lore.compiler.utils.CollectionExtensions._

class StructDefinition(
  override val name: String,
  override val tpe: StructType,
  // TODO: Reimplement ownedBy.
  //val ownedBy: Option[Type],
  val members: Vector[MemberDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {
  // Many of the members here are declared as vals. This is only possible because definitions are created according
  // to the inheritance hierarchy.

  /**
    * The list of all properties belonging to this struct.
    */
  val properties: Vector[PropertyDefinition] = members.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this struct.
    */
  val components: Vector[ComponentDefinition] = members.filterType[ComponentDefinition]

  /**
    * The signature of the call-style constructor.
    */
  lazy val constructorSignature: FunctionSignature = FunctionSignature(name, members.map(_.asParameter), tpe, position)

  /**
    * Returns all component types that this struct has in common with the other given struct.
    *
    * If two components match, but one is a supertype of the other, the supertype will be chosen.
    *
    * TODO: We should probably move this to DeclaredType or DeclaredTypeDefinition.
    */
  def commonComponentTypes(other: StructDefinition): Vector[ComponentType] = {
    components.map(_.tpe).flatMap { left =>
      val commonTypes = other.components.map(_.tpe).flatMap { right =>
        if (left <= right) Some(right)
        else if (left >= right) Some(left)
        else None
      }

      // There should only be exactly one type that the other entity has in common with this entity. If not, one of
      // the entities is violating the component subtyping hierarchy constraint.
      if (commonTypes.size > 1) {
        throw CompilationException(s"For component ${left.name} in struct $name, there are multiple components" +
          s" in common in struct ${other.name}: ${commonTypes.map(_.name).mkString(", ")}. Hence, we have a violation" +
          s" of the component subtyping hierarchy constraint.")
      }

      commonTypes.map(ComponentType)
    }
  }
}
