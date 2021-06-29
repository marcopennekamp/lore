package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.syntax.TypeDeclNode

object StructDefinitionResolver {

  def resolve(node: TypeDeclNode.StructNode)(implicit typeScope: TypeScope): Compilation[StructDefinition] = {
    val structType = typeScope.getStructType(node.name).getOrElse(
      throw CompilationException(s"The struct type for struct ${node.name} should be registered by now.")
    )

    node.properties.map(resolveProperty).simultaneous.map { properties =>
      val definition = new StructDefinition(node.name, structType, properties, node.position)
      structType.initialize(definition)
      definition
    }
  }

  case class MutableOpenProperty(node: TypeDeclNode.PropertyNode) extends Feedback.Error(node) {
    override def message = s"Open properties may not be mutable."
  }

  private def resolveProperty(node: TypeDeclNode.PropertyNode)(implicit typeScope: TypeScope): Compilation[StructPropertyDefinition] = {
    TypeExpressionEvaluator.evaluate(node.tpe).flatMap { tpe =>
      if (node.isOpen && node.isMutable) {
        Compilation.fail(MutableOpenProperty(node))
      } else {
        Compilation.succeed(new StructPropertyDefinition(node.name, tpe, node.isOpen, node.isMutable, node.defaultValue, node.position))
      }
    }
  }

}
