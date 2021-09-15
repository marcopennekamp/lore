package lore.compiler.semantics.scopes

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.structures.StructConstructor
import lore.compiler.types.{StructType, Type, TypeVariable}
import lore.compiler.types.TypeVariable.Assignments

sealed trait StructBinding extends Binding {
  /**
    * Whether the struct binding also has a companion module. Because modules are also resolved via binding scopes,
    * we need a means to tell certain parts of transformation that the struct constructor or object binding also has a
    * possible module component that can be accessed.
    */
  def hasCompanionModule: Boolean
}

/**
  * A struct constructor binding represents constructors as bindings and allows instantiating a specific underlying
  * struct type's constructor. The instantiation may require a type parameter list that is different from the struct
  * schema's original type parameters. This is due to the ability of (parameterized) type aliases to be used as
  * constructor names.
  */
case class StructConstructorBinding(
  name: NamePath,
  parameters: Vector[TypeVariable],
  underlyingType: StructType,
  hasCompanionModule: Boolean,
) extends StructBinding {
  val isConstant: Boolean = parameters.isEmpty

  def instantiate(assignments: Assignments): StructConstructor = {
    Type.substitute(underlyingType, assignments).asInstanceOf[StructType].constructor
  }

  override def toString: String = {
    val parameterString = if (parameters.nonEmpty) s"[${parameters.mkString(", ")}]" else ""
    s"$name$parameterString"
  }
}

case class StructObjectBinding(
  name: String,
  tpe: StructType,
  hasCompanionModule: Boolean,
) extends StructBinding with TypedBinding
