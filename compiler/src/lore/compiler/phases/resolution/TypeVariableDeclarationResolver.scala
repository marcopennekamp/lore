package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.FoldCompilationsExtension
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
    val compilation = nodes.foldCompiled((new LocalTypeScope(parentScope), 0)) { case ((typeScope, order), node) =>
      for {
        variable <- TypeVariableDeclarationResolver.resolve(node, order)(typeScope)
        _ <- typeScope.register(variable, node.position)
      } yield (typeScope, order + 1)
    }
    compilation.map(_._1)
  }

  /**
    * Resolves a single type variable declaration in the context of the given type scope.
    */
  private def resolve(node: DeclNode.TypeVariableNode, order: Int)(implicit typeScope: TypeScope): Compilation.Result[TypeVariable] = {
    for {
      lowerBound <- node.lowerBound.map(TypeExpressionEvaluator.evaluate(_, BasicType.Nothing)).toCompiledOption
      upperBound <- node.upperBound.map(TypeExpressionEvaluator.evaluate(_, BasicType.Any)).toCompiledOption
    } yield new TypeVariable(node.name, lowerBound.getOrElse(BasicType.Nothing), upperBound.getOrElse(BasicType.Any), order)
  }

}
