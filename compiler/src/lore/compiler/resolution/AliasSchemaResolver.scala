package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.{StructBinding, StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.modules.GlobalModule
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
    Resolver.withTypeParameters(schema.localModule, schema.node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val originalType = TypeExpressionEvaluator.evaluate(schema.node.tpe).getOrElse(BasicType.Any)
        schema.initialize(typeParameters, originalType)
    }
  }

  /**
    * Initializes `structBinding`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initializeStructBinding(schema: AliasSchema, structBinding: StructBinding): Unit = schema.originalType match {
    case underlyingType: StructType =>
      val underlyingSchema = underlyingType.schema
      structBinding match {
        case binding: StructConstructorBinding => binding.initialize(underlyingType)
        case binding: StructObjectBinding => binding.initialize(underlyingSchema)
      }
    case _ =>
  }

}
