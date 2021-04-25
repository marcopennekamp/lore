package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error, Errors, Result}
import lore.compiler.phases.transformation.inference.Inference
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.scopes.{TypeScope, VariableScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.types.{ProductType, Type}

object ExpressionTransformer {

  /**
    * Builds a semantic expression tree from the given AST expression node, performing type inference in the process.
    * Ensures that all other expression constraints hold.
    */
  def transform(
    name: String,
    node: ExprNode,
    expectedType: Type,
    typeScope: TypeScope,
    variableScope: VariableScope,
  )(implicit registry: Registry): Compilation[Expression] = {
    val visitor = new InferringExpressionTransformationVisitor(expectedType, typeScope, variableScope)

    for {
      expression <- TopLevelExprVisitor.visitCompilation(visitor)(node)

      _ = {
        println(s"Typing judgments for $name:")
        visitor.typingJudgments.foreach(println)
        println()
      }
      inferredTypes <- Inference.infer(visitor.typingJudgments) match {
        case result@Result(_, _) => result
        case errors@Errors(_, _) =>
          println("Inference failed!")
          println()
          println()
          errors
      }
      _ = {
        println("Inferred types:")
        println(inferredTypes)
        println()
        println()
      }

      rehydrationVisitor = new TypeRehydrationVisitor(inferredTypes)
      typedExpression = ExpressionVisitor.visit(rehydrationVisitor)(expression)

      mutabilityVerifier = new MutabilityVerifier
      _ <- ExpressionVisitor.visitCompilation(mutabilityVerifier)(typedExpression)

      builtinsVisitor = new BuiltinsVisitor
      expressionWithBuiltins <- ExpressionVisitor.visitCompilation(builtinsVisitor)(typedExpression)

      expressionWithImplicitUnit = withImplicitUnitValue(expectedType)(expressionWithBuiltins)

      _ <- verifyExpectedType(expressionWithImplicitUnit, expectedType)
    } yield expressionWithImplicitUnit
  }

  /**
    * For a block expression expected to return Unit, we have to manually add a unit return value if the block's value
    * isn't already a unit value.
    *
    * Example:
    * {{{
    * action test() {
    *   concat([12], [15]) // Should compile even though it returns a list.
    * }
    * }}}
    */
  private def withImplicitUnitValue(expectedType: Type)(expression: Expression): Expression = {
    expression match {
      case Expression.Block(expressions, position) if expectedType == ProductType.UnitType && expression.tpe != ProductType.UnitType =>
        Expression.Block(expressions :+ Expression.Tuple(Vector.empty, position), position)
      case _ => expression
    }
  }

  // TODO: Move this error somewhere else. This might also be a good candidate for being merged with other errors...

  case class IllegallyTypedExpression(expression: Expression, expectedTypes: Vector[Type]) extends Error(expression) {
    override def message = s"The expression $expression has the illegal type ${expression.tpe}.$expected"

    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types (or a subtype thereof): ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  /**
    * Verifies that the expected result type is compatible with the type of the expression. If the actual type is not
    * compatible, it might be the case that all paths of the expression's last expression return a valid value. In such
    * a case, the expression is valid as well, because we can guarantee at compile-time that the right kind of value
    * is returned before the end of the expression is reached. This special case is also handled by this verification.
    */
  private def verifyExpectedType(expression: Expression, expectedType: Type): Verification = {
    if (expression.tpe <= expectedType || allPathsReturn(expression)) {
      Verification.succeed
    } else {
      Compilation.fail(IllegallyTypedExpression(expression, Vector(expectedType)))
    }
  }

  /**
    * Whether all paths that could be taken during the evaluation of the expression definitely end in a return.
    * If that is the case, and such an expression is the last expression in a block, we can be sure that the
    * block evaluates to the correct value regardless of the type of the actual expression.
    *
    * We only look at the last expression of a block to decide whether the returns suffice. That is only valid because
    * we combine it with dead code analysis, with dead code resulting in an error. A function like the following could
    * thus never be valid:
    *
    * {{{
    * function foo(): Int = {
    *   return 5
    *   'You fool!'
    * }
    * }}}
    */
  private def allPathsReturn(expression: Expression): Boolean = {
    expression match {
      case Expression.Return(_, _) => true
      case Expression.Block(expressions, _) => expressions.lastOption.exists(allPathsReturn)
      case Expression.IfElse(_, onTrue, onFalse, _, _) => allPathsReturn(onTrue) && allPathsReturn(onFalse)

      // Loops aren't guaranteed to run even once and so cannot guarantee that all paths end in a return.
      // Hence Expression.WhileLoop and Expression.ForLoop will also result in false.
      case _ => false
    }
  }

}
