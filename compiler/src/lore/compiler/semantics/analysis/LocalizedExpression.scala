package lore.compiler.semantics.analysis

import lore.compiler.semantics.expressions.{Expression, ExpressionVerificationVisitor, ExpressionVisitor}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.semantics.scopes.{StructObjectBinding, Variable}
import lore.compiler.semantics.structures.StructConstructor
import lore.compiler.semantics.variables.GlobalVariableDefinition

object LocalizedExpression {

  /**
    * A localized expression doesn't access any external functions, constructors, objects, or global variables. The
    * property is used to decide whether objects and global variables need to be initialized lazily.
    *
    * The algorithm is currently very conservative: any usage of a function, struct, or global variable leads to lazy
    * initialization.
    */
  def isLocalized(expression: Expression): Boolean = {
    val visitor = LocalizedVisitor()
    ExpressionVisitor.visit(visitor)(expression)
    visitor.isLocalized
  }

  private case class LocalizedVisitor(var isLocalized: Boolean = true) extends ExpressionVerificationVisitor {
    private def setFalse(): Unit = isLocalized = false

    override def verify(expression: Expression): Unit = expression match {
      case Expression.BindingAccess(binding, _) => binding match {
        case _: GlobalVariableDefinition => setFalse()
        case StructConstructor(_) => setFalse()
        case StructObjectBinding(_, _) => setFalse()
        case Variable(_, _, _) =>
      }
      case _: Expression.MultiFunctionValue => setFalse()
      case _: Expression.FixedFunctionValue => setFalse()
      case Expression.Call(target, _, _, _) => target match {
        case CallTarget.Value(_) =>
        case CallTarget.MultiFunction(_) => setFalse()
        case CallTarget.Dynamic(_) => setFalse()
      }
      case _ =>
    }
  }

}
