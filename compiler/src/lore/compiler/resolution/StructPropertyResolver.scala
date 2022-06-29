package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.syntax.DeclNode.PropertyNode
import lore.compiler.types.{BasicType, StructSchema}

object StructPropertyResolver {

  def resolve(
    schema: StructSchema,
    propertyNodes: Vector[PropertyNode],
  )(implicit registry: Registry, reporter: Reporter): Unit = {
    val definition = schema.definition
    Resolver.withRegistryScopes(definition.localModule) {
      registryTypeScope => implicit termScope =>
        implicit val typeScope: TypeScope = schema.getTypeScope(registryTypeScope)
        val properties = propertyNodes.map(resolveProperty)
        definition.propertiesOnce.assign(properties)
        properties.foreach(_.structDefinition = definition)
    }
  }

  // TODO (multi-import): Move error to central location.
  case class MutableOpenProperty(node: PropertyNode) extends Feedback.Error(node) {
    override def message = s"The open property ${node.name} may not be mutable."
  }

  private def resolveProperty(node: PropertyNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): StructPropertyDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    if (node.isOpen && node.isMutable) {
      reporter.error(MutableOpenProperty(node))
    }
    new StructPropertyDefinition(node.name, tpe, node.isOpen, node.isMutable, node.defaultValue, node.nameNode.position)
  }

}
