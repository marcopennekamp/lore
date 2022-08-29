package lore.compiler.semantics.bindings

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.StructConstructorBinding.InstantiationSchema
import lore.compiler.semantics.definitions.{BindingDefinitionKind, TermDefinition, TypeDefinition}
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.types._
import lore.compiler.utils.Once

// TODO: We could roll StructObjectBindings into GlobalVariableDefinitions, that is, generating a semantic global
//       variable for each struct object. This might simplify the whole StructBinding business.

/**
  * A struct binding represents constructors and objects as terms. A struct binding may have a different underlying
  * schema than its defining schema due to the ability of (parameterized) type aliases to be used as constructors and
  * objects.
  *
  * Because type aliases point to an underlying schema that might not have been created yet during creation of the
  * struct binding, the underlying schema has to be resolved during initialization of the struct binding. (Also compare
  * the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
  */
sealed trait StructBinding extends TermDefinition {
  private val _underlyingSchema: Once[StructSchema] = new Once

  def definingSchema: TypeDefinition
  def underlyingSchema: StructSchema = _underlyingSchema

  def initialize(underlyingSchema: StructSchema): Unit = {
    _underlyingSchema.assign(underlyingSchema)
  }

  override def isInitialized: Boolean = _underlyingSchema.isAssigned

  override def name: NamePath = definingSchema.name
  override def definitionKind: BindingDefinitionKind = BindingDefinitionKind.Struct
  override def position: Position = definingSchema.position
}

/**
  * A struct constructor binding allows instantiating a specific underlying struct type and its constructor.
  */
class StructConstructorBinding(
  override val definingSchema: TypeDefinition,
) extends StructBinding {
  private val _underlyingType: Once[StructType] = new Once

  def underlyingType: StructType = _underlyingType
  def typeParameters: Vector[TypeVariable] = definingSchema.typeParameters

  def initialize(underlyingType: StructType): Unit = {
    super.initialize(underlyingType.schema)
    _underlyingType.assign(underlyingType)
  }

  override def isInitialized: Boolean = super.isInitialized && _underlyingType.isAssigned

  lazy val isConstant: Boolean = typeParameters.isEmpty

  lazy val signature: FunctionSignature = underlyingType.constructorSignature.copy(typeParameters = typeParameters)

  private lazy val instantiationSchema = InstantiationSchema(name, typeParameters, underlyingType)

  def instantiateStructType(assignments: Assignments): StructType = instantiationSchema.instantiate(assignments)

  def instantiateStructType(
    arguments: Vector[Option[Type]],
    position: Position,
  )(implicit reporter: Reporter): StructType = {
    instantiationSchema.instantiate(arguments, position)
  }

  override def toString: String = {
    val typeParameterString = if (isInitialized && typeParameters.nonEmpty) {
      s"[${typeParameters.mkString(", ")}]"
    } else ""
    s"$name$typeParameterString"
  }
}

object StructConstructorBinding {
  /**
    * This is a private helper schema to instantiate a struct type for the struct constructor binding without much code
    * duplication.
    */
  private case class InstantiationSchema(
    name: NamePath,
    typeParameters: Vector[TypeVariable],
    underlyingType: StructType,
  ) extends NamedSchema {
    override def instantiate(assignments: Assignments): StructType = {
      Type.substitute(underlyingType, assignments).asInstanceOf[StructType]
    }

    override def instantiate(
      typeArguments: Vector[Option[Type]],
      position: Position,
    )(implicit reporter: Reporter): StructType = {
      super.instantiate(typeArguments, position).asInstanceOf[StructType]
    }
  }
}

class StructObjectBinding(
  override val definingSchema: TypeDefinition,
) extends StructBinding with TypedTermBinding {
  private val _underlyingType: Once[StructType] = new Once

  def underlyingType: StructType = _underlyingType
  override def tpe: Type = underlyingType

  override def initialize(underlyingSchema: StructSchema): Unit = {
    super.initialize(underlyingSchema)
    _underlyingType.assign(underlyingSchema.constantType)
  }

  override def isInitialized: Boolean = _underlyingType.isAssigned
}
