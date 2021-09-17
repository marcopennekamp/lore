package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.scopes.StructConstructorBinding.StructConstructorBindingSchema
import lore.compiler.semantics.structures.StructConstructor
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.types.{NamedSchema, StructType, Type, TypeVariable}

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
  lazy val asSchema: StructConstructorBindingSchema = StructConstructorBindingSchema(name, parameters, underlyingType)

  def instantiate(assignments: Assignments): StructConstructor = {
    asSchema.instantiate(assignments).constructor
  }

  override def toString: String = {
    val parameterString = if (parameters.nonEmpty) s"[${parameters.mkString(", ")}]" else ""
    s"$name$parameterString"
  }
}

object StructConstructorBinding {
  case class StructConstructorBindingSchema(name: NamePath, parameters: Vector[TypeVariable], underlyingType: StructType) extends NamedSchema {
    override def instantiate(assignments: Assignments): StructType = Type.substitute(underlyingType, assignments).asInstanceOf[StructType]
    override def instantiate(arguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): StructType = {
      super.instantiate(arguments, position).asInstanceOf[StructType]
    }
  }
}

case class StructObjectBinding(
  name: NamePath,
  tpe: StructType,
  hasCompanionModule: Boolean,
) extends StructBinding with TypedBinding
