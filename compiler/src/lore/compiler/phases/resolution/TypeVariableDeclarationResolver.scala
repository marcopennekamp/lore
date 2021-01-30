package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.semantics.scopes.{LocalTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, TypeVariable}

object TypeVariableDeclarationResolver {

  /**
    * Resolves a type variable declaration list in order, ensuring that the order property of the type variables is
    * set correctly.
    */
  def resolve(nodes: Vector[DeclNode.TypeVariableNode], parentScope: TypeScope): Compilation[LocalTypeScope] = {
    // The fold ensures that the first type variable is registered before the second one is resolved, so that the first
    // one can be used as a bound of the second one, and so on.
    val initial = (new LocalTypeScope(parentScope).compiled, 0)
    val (compilation, _) = nodes.foldLeft(initial) { case ((compilation, order), node) =>
      implicit val position: Position = node.position
      val nextCompilation = compilation.flatMap { implicit typeScope =>
        TypeVariableDeclarationResolver.resolve(node, order).flatMap { variable =>
          typeScope.register(variable).map(_ => typeScope)
        }
      }
      (nextCompilation, order + 1)
    }
    compilation
  }

  /**
    * Resolves a single type variable declaration in the context of the given type scope.
    */
  def resolve(node: DeclNode.TypeVariableNode, order: Int)(implicit typeScope: TypeScope): Compilation[TypeVariable] = {
    for {
      lowerBound <- node.lowerBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      upperBound <- node.upperBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
    } yield new TypeVariable(node.name, lowerBound.getOrElse(BasicType.Nothing), upperBound.getOrElse(BasicType.Any), order)
  }

}
