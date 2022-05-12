package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableConstraints {

  /**
    * Verifies:
    *   - Expression constraints for the global variable's value expression.
    */
  def verify(variable: GlobalVariableDefinition)(implicit reporter: Reporter): Unit = {
    ExpressionConstraints.verify(variable.valueNode)
  }

}
