package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.scopes.StructConstructorBinding.StructConstructorBindingSchema
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.types.{NamedSchema, StructType, Type, TypeVariable}

sealed trait StructBinding extends Binding {
  def definition: StructDefinition
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
) extends StructBinding {
  val isConstant: Boolean = parameters.isEmpty
  lazy val asSchema: StructConstructorBindingSchema = StructConstructorBindingSchema(name, parameters, underlyingType)
  lazy val signature: FunctionSignature = underlyingType.constructorSignature

  override val definition: StructDefinition = underlyingType.schema.definition

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

case class StructObjectBinding(name: NamePath, tpe: StructType) extends StructBinding with TypedBinding {
  override val definition: StructDefinition = tpe.schema.definition
}
