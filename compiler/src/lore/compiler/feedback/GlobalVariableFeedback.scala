package lore.compiler.feedback

import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableFeedback {

  case class AlreadyExists(variable: GlobalVariableDefinition) extends Feedback.Error(variable) {
    override def message: String = s"The global variable ${variable.name} is already declared somewhere else."
  }

}
