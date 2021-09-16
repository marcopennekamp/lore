package lore.compiler.feedback

import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableFeedback {
  case class NameTaken(variable: GlobalVariableDefinition) extends Feedback.Error(variable) {
    override def message: String = s"The name of the global variable ${variable.name} is already taken by a module or" +
      s" multi-function. Modules, global variables, and multi-functions may not share names."
  }

  case class AlreadyExists(variable: GlobalVariableDefinition) extends Feedback.Error(variable) {
    override def message: String = s"The global variable ${variable.name} is already declared somewhere else."
  }
}
