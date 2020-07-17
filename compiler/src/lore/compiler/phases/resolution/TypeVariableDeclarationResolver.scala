package lore.compiler.phases.resolution

import lore.compiler.ast.DeclNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, Fragment, TypeScope, TypeVariableScope}
import lore.compiler.feedback.Position
import lore.compiler.types.{AnyType, NothingType, TypeExpressionEvaluator, TypeVariable}

object TypeVariableDeclarationResolver {
  /**
    * Resolves a type variable declaration list in order, ensuring that the order property of the type variables is
    * set correctly.
    */
  def resolve(nodes: List[DeclNode.TypeVariableNode])(implicit typeScope: TypeScope, fragment: Fragment): C[TypeVariableScope] = {
    // The fold ensures that the first type variable is registered before the second one is resolved, so that the first
    // one can be used as a bound of the second one, and so on.
    val initial = (Compilation.succeed(new TypeVariableScope(typeScope)), 0)
    val (compilation, _) = nodes.foldLeft(initial) { case ((compilation, order), node) =>
      implicit val position: Position = node.position
      val nextCompilation = compilation.flatMap { implicit typeScope =>
        TypeVariableDeclarationResolver.resolve(node, order).map { variable =>
          typeScope.register(variable)
          typeScope
        }
      }
      (nextCompilation, order + 1)
    }
    compilation
  }

  /**
    * Resolves a single type variable declaration.
    */
  def resolve(node: DeclNode.TypeVariableNode, order: Int)(implicit typeScope: TypeScope, fragment: Fragment): C[TypeVariable] = {
    for {
      lowerBound <- node.lowerBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      upperBound <- node.upperBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
    } yield new TypeVariable(node.name, lowerBound.getOrElse(NothingType), upperBound.getOrElse(AnyType), order)
  }
}
