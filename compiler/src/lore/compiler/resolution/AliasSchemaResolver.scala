package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.structures.{StructBinding, StructConstructorBinding, StructObjectBinding}
import lore.compiler.syntax.DeclNode.AliasNode
import lore.compiler.types.AliasSchema.AliasVariant
import lore.compiler.types._

object AliasSchemaResolver {

  /**
    * Creates an uninitialized [[AliasSchema]] for `node`. Local modules of nodes are not yet resolved at this point.
    */
  def create(
    node: AliasNode,
    globalModule: GlobalModule,
  )(implicit registry: Registry, reporter: Reporter): AliasSchema = {
    new AliasSchema(globalModule.name + node.simpleName, node.aliasVariant, node)
  }

  /**
    * Creates an uninitialized [[StructBinding]] for `schema`, if it is a struct alias. Local modules of nodes are not
    * yet resolved at this point.
    */
  def createStructBinding(schema: AliasSchema): StructBinding = schema.aliasVariant match {
    case AliasVariant.Struct => new StructConstructorBinding(schema)
    case AliasVariant.Object => new StructObjectBinding(schema)
    case _ => throw CompilationException(s"Cannot create a struct binding for `type` alias `${schema.name}`.")
  }

  /**
    * Initializes `schema`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initialize(schema: AliasSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    TypeResolver.withTypeParameters(schema.localModule, schema.node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val originalType = TypeResolver.resolve(schema.node.tpe).getOrElse(BasicType.Any)
        schema.initialize(typeParameters, originalType)
    }
  }

  /**
    * Initializes `structBinding`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    *
    * If the alias's original type is not a struct type or if the alias is an object and the underlying type is not,
    * the struct binding is initialized with a mock struct or object. This ensures that the compiler can continue
    * working with an initialized struct binding, which is necessary to avoid compiler crashes. Note that the alias
    * struct <i>binding</i> is initialized with the mock type, not the type alias itself, which retains its original
    * type.
    */
  def initializeStructBinding(schema: AliasSchema, structBinding: StructBinding): Unit = {
    val underlyingType = schema.originalType match {
      case underlyingType: StructType if !schema.isObjectAlias || underlyingType.schema.isObject => underlyingType
      case _ => createMockStruct(schema)
    }

    structBinding match {
      case binding: StructConstructorBinding => binding.initialize(underlyingType)
      case binding: StructObjectBinding => binding.initialize(underlyingType.schema)
    }
  }

  /**
    * Fallback-initializes `schema`, initializing type parameters without bounds and ignoring the underlying type
    * expression. Regular aliases are initialized to `Any`, while struct aliases are initialized with a mock struct or
    * object.
    */
  def fallbackInitialize(schema: AliasSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    TypeResolver.withTypeParameters(schema.localModule, schema.node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val originalType = schema.aliasVariant match {
          case AliasVariant.Type => BasicType.Any
          case AliasVariant.Struct | AliasVariant.Object => createMockStruct(schema)
        }
        schema.initialize(typeParameters, originalType)
    }
  }

  private def createMockStruct(schema: AliasSchema): StructType = {
    StructSchema.createMock(schema.name, schema.aliasVariant == AliasVariant.Object).constantType
  }

}
