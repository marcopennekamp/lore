package lore.compiler.semantics.analysis

import lore.compiler.core.{CompilationException, UniqueKey}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.Expression.{AnonymousFunction, ForLoop}
import lore.compiler.semantics.expressions.{Expression, ExpressionCombiningVisitor, ExpressionVisitor}

object CapturedVariables {

  /**
    * Finds all unique local variables that the given lambda expression must invariably capture.
    */
  def findCapturedVariables(expression: Expression.AnonymousFunction): Set[LocalVariable] = {
    val visitor = CapturedVariableVisitor()
    ExpressionVisitor.visit(visitor)(expression)
  }

  private case class CapturedVariableVisitor() extends ExpressionCombiningVisitor[Set[LocalVariable], Set[LocalVariable]] {
    // The set of variables which are declared locally and don't need to be captured. Because local variables have
    // unique keys, we can disregard scopes here. The parameters of the analyzed anonymous function are handled in the
    // `before` function.
    private var localDeclarations: Set[UniqueKey] = Set.empty

    override def combine(values: Vector[Set[LocalVariable]]): Set[LocalVariable] = values.flatten.toSet

    override def visit(expression: Expression.VariableDeclaration)(capturedVariables: Set[LocalVariable]): Set[LocalVariable] = {
      localDeclarations += expression.variable.uniqueKey
      capturedVariables
    }

    override def visit(expression: Expression.BindingAccess): Set[LocalVariable] = {
      expression.binding match {
        case variable: LocalVariable if !localDeclarations.contains(variable.uniqueKey) =>
          if (variable.isMutable) {
            throw CompilationException(s"Cannot capture mutable variable `${variable.name}`: not implemented yet. Position: ${expression.position}.")
          }
          Set(variable)
        case _ => Set.empty
      }
    }

    override def before: PartialFunction[Expression, Unit] = {
      case expression: AnonymousFunction =>
        // This also registers the parameters of the outer-most anonymous function.
        localDeclarations ++= expression.parameters.map(_.uniqueKey)

      case expression: ForLoop =>
        localDeclarations ++= expression.extractors.map(_.variable.uniqueKey)
    }
  }

}
