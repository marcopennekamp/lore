package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, TypeVariable}

object TypeVariableResolver {

  /**
    * Resolves a type variable declaration list in order.
    */
  def resolve(nodes: Vector[DeclNode.TypeVariableNode], parentScope: TypeScope)(implicit reporter: Reporter): Vector[TypeVariable] = {
    nodes.foldLeft(Vector.empty[TypeVariable]) { case (typeVariables, node) =>
      implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeVariables, parentScope)
      typeVariables :+ resolve(node)
    }
  }

  /**
    * Resolves a single type variable declaration in the context of the given type scope.
    */
  private def resolve(node: DeclNode.TypeVariableNode)(implicit typeScope: TypeScope, reporter: Reporter): TypeVariable = {
    val lowerBound = node.lowerBound.flatMap(TypeExpressionEvaluator.evaluate).getOrElse(BasicType.Nothing)
    val upperBound = node.upperBound.flatMap(TypeExpressionEvaluator.evaluate).getOrElse(BasicType.Any)
    new TypeVariable(node.name, lowerBound, upperBound, node.variance)
  }

}
