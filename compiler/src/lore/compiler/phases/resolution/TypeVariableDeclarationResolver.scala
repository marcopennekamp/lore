package lore.compiler.phases.resolution

import lore.compiler.ast.DeclNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.Fragment
import lore.compiler.definitions.TypeScope
import lore.compiler.types.{TypeExpressionEvaluator, TypeVariable}
import lore.types.{AnyType, NothingType}

object TypeVariableDeclarationResolver {
  /**
    * Resolves a single type variable declaration.
    */
  def resolve(node: DeclNode.TypeVariableNode)(implicit typeScope: TypeScope, fragment: Fragment): C[TypeVariable] = {
    for {
      lowerBound <- node.lowerBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      upperBound <- node.upperBound.map(TypeExpressionEvaluator.evaluate).toCompiledOption
    } yield new TypeVariable(node.name, lowerBound.getOrElse(NothingType), upperBound.getOrElse(AnyType))
  }
}
