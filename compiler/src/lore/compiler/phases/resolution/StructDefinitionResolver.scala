package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, CompilationException, Position}
import lore.compiler.semantics.structures.{ComponentDefinition, PropertyDefinition, PropertyDefinitionImpl, StructDefinition}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.BasicType

object StructDefinitionResolver {
  def resolve(node: TypeDeclNode.StructNode)(implicit registry: Registry): Compilation[StructDefinition] = {
    implicit val position: Position = node.position
    implicit val typeScope: TypeScope = registry.typeScope
    val structType = registry.getStructType(node.name).getOrElse(
      throw CompilationException(s"The struct type for struct ${node.name} should be registered by now.")
    )

    (
      node.ownedBy.map(TypeExpressionEvaluator.evaluate).toCompiledOption.map(_.getOrElse(BasicType.Any)),
      node.properties.map(resolveMember).simultaneous,
    ).simultaneous.map { case (ownedBy, members) =>
      val definition = new StructDefinition(node.name, structType, ownedBy, members, node.position)
      structType.initialize(definition)
      definition
    }
  }

  private def resolveMember(node: TypeDeclNode.MemberNode)(implicit typeScope: TypeScope): Compilation[PropertyDefinition] = {
    node match {
      case TypeDeclNode.PropertyNode(name, tpe, isMutable, defaultValue, _) =>
        for {
          tpe <- TypeExpressionEvaluator.evaluate(tpe)
        } yield new PropertyDefinitionImpl(name, tpe, isMutable, defaultValue, node.position)
      case node@TypeDeclNode.ComponentNode(name, defaultValue, position) =>
        TypeResolver.resolveComponentType(node).map(tpe => new ComponentDefinition(name, tpe, defaultValue, position))
    }
  }
}
