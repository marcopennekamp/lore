package lore.compiler.semantics.bindings

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.StructConstructorBinding.InstantiationSchema
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.types.{NamedSchema, StructType, Type, TypeVariable}

// TODO: We could roll StructObjectBindings into GlobalVariableDefinitions, that is, generating a semantic global
//       variable for each struct object. This might simplify the whole StructBinding business.

sealed trait StructBinding extends TermBinding {
  def definition: StructDefinition
}

/**
  * A struct constructor binding represents constructors as terms and allows instantiating a specific underlying struct
  * type and its constructor. The instantiation may require a type parameter list that is different from the struct
  * schema's original type parameters. This is due to the ability of (parameterized) type aliases to be used as
  * constructor names.
  */
case class StructConstructorBinding(
  name: NamePath,
  typeParameters: Vector[TypeVariable],
  underlyingType: StructType,
) extends StructBinding {
  val isConstant: Boolean = typeParameters.isEmpty

  override val definition: StructDefinition = underlyingType.schema.definition

  lazy val signature: FunctionSignature = underlyingType.constructorSignature.copy(typeParameters = typeParameters)

  private lazy val instantiationSchema = InstantiationSchema(name, typeParameters, underlyingType)

  def instantiateStructType(assignments: Assignments): StructType = instantiationSchema.instantiate(assignments)

  def instantiateStructType(arguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): StructType = {
    instantiationSchema.instantiate(arguments, position)
  }

  override def toString: String = {
    val typeParameterString = if (typeParameters.nonEmpty) s"[${typeParameters.mkString(", ")}]" else ""
    s"$name$typeParameterString"
  }
}

object StructConstructorBinding {

  /**
    * This is a private helper schema to instantiate a struct type for the struct constructor binding without much code
    * duplication.
    */
  private case class InstantiationSchema(name: NamePath, parameters: Vector[TypeVariable], underlyingType: StructType) extends NamedSchema {
    override def instantiate(assignments: Assignments): StructType = Type.substitute(underlyingType, assignments).asInstanceOf[StructType]
    override def instantiate(arguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): StructType = {
      super.instantiate(arguments, position).asInstanceOf[StructType]
    }
  }

}

case class StructObjectBinding(name: NamePath, tpe: StructType) extends StructBinding with TypedTermBinding {
  override val definition: StructDefinition = tpe.schema.definition
}
