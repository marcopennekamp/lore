package lore.compiler.phases.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.BasicType

object StructDefinitionResolver {

  def resolve(node: TypeDeclNode.StructNode, parentScope: TypeScope)(implicit reporter: Reporter): StructDefinition = {
    val schema = parentScope.getStructSchema(node.name).getOrElse(
      throw CompilationException(s"The type schema for struct ${node.name} should be registered by now.")
    )
    implicit val typeScope: TypeScope = schema.typeScope

    val properties = node.properties.map(resolveProperty)
    val definition = new StructDefinition(node.name, schema, properties, node.nameNode.position)
    schema.initialize(definition)
    definition
  }

  case class MutableOpenProperty(node: TypeDeclNode.PropertyNode) extends Feedback.Error(node) {
    override def message = s"Open properties may not be mutable."
  }

  private def resolveProperty(node: TypeDeclNode.PropertyNode)(implicit typeScope: TypeScope, reporter: Reporter): StructPropertyDefinition = {
    val tpe = TypeExpressionEvaluator.evaluate(node.tpe).getOrElse(BasicType.Any)
    if (node.isOpen && node.isMutable) {
      reporter.error(MutableOpenProperty(node))
    }
    new StructPropertyDefinition(node.name, tpe, node.isOpen, node.isMutable, node.defaultValue, node.nameNode.position)
  }

}
