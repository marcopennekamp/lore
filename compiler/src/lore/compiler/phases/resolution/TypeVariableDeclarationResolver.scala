package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.{LocalTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, TypeVariable}

object TypeVariableDeclarationResolver {

  /**
    * Resolves a type variable declaration list in order, ensuring that the order property of the type variables is
    * set correctly.
    */
  def resolve(nodes: Vector[DeclNode.TypeVariableNode], parentScope: TypeScope)(implicit reporter: Reporter): LocalTypeScope = {
    implicit val typeScope: LocalTypeScope = new LocalTypeScope(parentScope)
    nodes.foldLeft(0) { case (order, node) =>
      val variable = resolve(node, order)
      typeScope.register(variable, node.position)
      order + 1
    }
    typeScope
  }

  /**
    * Resolves a single type variable declaration in the context of the given type scope.
    */
  private def resolve(node: DeclNode.TypeVariableNode, order: Int)(implicit typeScope: TypeScope, reporter: Reporter): TypeVariable = {
    val lowerBound = node.lowerBound.flatMap(TypeExpressionEvaluator.evaluate).getOrElse(BasicType.Nothing)
    val upperBound = node.upperBound.flatMap(TypeExpressionEvaluator.evaluate).getOrElse(BasicType.Any)
    new TypeVariable(node.name, lowerBound, upperBound, order)
  }

}
