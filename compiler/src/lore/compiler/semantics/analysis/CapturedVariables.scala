package lore.compiler.semantics.analysis

import lore.compiler.core.{CompilationException, UniqueKey}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.typed.Expression.{BindingAccess, ForLoop, LambdaValue, VariableDeclaration}
import lore.compiler.semantics.expressions.typed.{Expression, ExpressionCombiningVisitor, ExpressionVisitor}

object CapturedVariables {

  /**
    * Finds all unique local variables that the given lambda expression must invariably capture.
    */
  def findCapturedVariables(expression: LambdaValue): Set[LocalVariable] = {
    val visitor = CapturedVariableVisitor()
    ExpressionVisitor.visit(visitor)(expression)
  }

  private case class CapturedVariableVisitor() extends ExpressionCombiningVisitor[Set[LocalVariable], Set[LocalVariable]] {
    // The set of variables which are declared locally and don't need to be captured. Because local variables have
    // unique keys, we can disregard scopes here. The parameters of the analyzed lambda function are handled in the
    // `before` function.
    private var localDeclarations: Set[UniqueKey] = Set.empty

    override def combine(values: Vector[Set[LocalVariable]]): Set[LocalVariable] = values.flatten.toSet

    override def visit(
      expression: VariableDeclaration,
    )(capturedVariables: Set[LocalVariable]): Set[LocalVariable] = {
      localDeclarations += expression.variable.uniqueKey
      capturedVariables
    }

    override def visit(expression: BindingAccess): Set[LocalVariable] = {
      expression.binding match {
        case variable: LocalVariable if !localDeclarations.contains(variable.uniqueKey) =>
          if (variable.isMutable) {
            throw CompilationException(s"Cannot capture mutable variable `${variable.name}`: not implemented yet." +
              s" Position: ${expression.position}.")
          }
          Set(variable)
        case _ => Set.empty
      }
    }

    override def before: PartialFunction[Expression, Unit] = {
      case expression: LambdaValue =>
        // This also registers the parameters of the outer-most lambda function.
        localDeclarations ++= expression.parameters.map(_.variable.uniqueKey)

      case expression: ForLoop =>
        localDeclarations ++= expression.extractors.map(_.variable.uniqueKey)
    }
  }

}
