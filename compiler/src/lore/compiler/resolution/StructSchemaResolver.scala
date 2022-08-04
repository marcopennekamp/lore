package lore.compiler.resolution

import lore.compiler.feedback.{Reporter, StructFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.{StructBinding, StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.DeclNode.{PropertyNode, StructNode}
import lore.compiler.types.{BasicType, StructProperty, StructSchema}

object StructSchemaResolver {

  /**
    * Creates an uninitialized [[StructSchema]] for `node`. Local modules of nodes are not yet resolved at this point.
    */
  def create(
    node: StructNode,
    globalModule: GlobalModule,
  )(implicit registry: Registry, reporter: Reporter): StructSchema = {
    new StructSchema(globalModule.name + node.simpleName, node.isObject, node)
  }

  /**
    * Creates an uninitialized [[StructBinding]] for `schema`. Local modules of nodes are not yet resolved at this
    * point.
    */
  def createStructBinding(schema: StructSchema): StructBinding = {
    if (schema.isObject) new StructObjectBinding(schema)
    else new StructConstructorBinding(schema)
  }

  /**
    * Initializes `structBinding`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initializeStructBinding(schema: StructSchema, structBinding: StructBinding): Unit = {
    structBinding match {
      case binding: StructConstructorBinding => binding.initialize(schema.instantiate(schema.identityAssignments))
      case binding: StructObjectBinding => binding.initialize(schema)
    }
  }

  def initializeProperties(schema: StructSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    Resolver.withRegistryScopes(schema.localModule) {
      registryTypeScope => implicit termScope =>
        implicit val typeScope: TypeScope = schema.getTypeScope(registryTypeScope)
        val properties = schema.node.properties.map(resolveProperty(schema, _))
        schema.initializeProperties(properties)
    }
  }

  private def resolveProperty(schema: StructSchema, node: PropertyNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): StructProperty = {
    val tpe = TypeResolver.resolve(node.tpe).getOrElse(BasicType.Any)
    if (node.isOpen && node.isMutable) {
      reporter.error(StructFeedback.MutableOpenProperty(node))
    }
    new StructProperty(
      node.name,
      tpe,
      node.isOpen,
      node.isMutable,
      node.defaultValue,
      schema,
      node.nameNode.position,
    )
  }

}
