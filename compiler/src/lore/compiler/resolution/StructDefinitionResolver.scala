package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.scopes.{BindingScope, TypeScope}
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, StructSchema}
import lore.compiler.utils.CollectionExtensions.OptionExtension

object StructDefinitionResolver {

  def resolve(node: DeclNode.StructNode)(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): StructDefinition = {
    Resolver.withRegistryScopes(node.localModule) {
      registryTypeScope => implicit bindingScope =>
        val schema = types.schemas.get(node.fullName).filterType[StructSchema].getOrElse(
          throw CompilationException(s"The type schema for struct ${node.fullName} should be registered by now.")
        )
        implicit val typeScope: TypeScope = schema.getTypeScope(registryTypeScope)

        val properties = node.properties.map(resolveProperty)
        val companionModule = bindings.modules.get(node.fullName)
        val definition = new StructDefinition(node.fullName, schema, properties, node.isObject, companionModule, node.localModule, node.nameNode.position)
        schema.initialize(definition)
        definition
    }
  }

  case class MutableOpenProperty(node: DeclNode.PropertyNode) extends Feedback.Error(node) {
    override def message = s"The open property ${node.name} may not be mutable."
  }

  private def resolveProperty(node: DeclNode.PropertyNode)(implicit typeScope: TypeScope, bindingScope: BindingScope, reporter: Reporter): StructPropertyDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    if (node.isOpen && node.isMutable) {
      reporter.error(MutableOpenProperty(node))
    }
    new StructPropertyDefinition(node.name, tpe, node.isOpen, node.isMutable, node.defaultValue, node.nameNode.position)
  }

}
