package lore.compiler.feedback

import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition

object MultiFunctionFeedback {

  case class NameTakenByVariable(mf: MultiFunctionDefinition, variable: GlobalVariableDefinition) extends Feedback.Error(mf.functions.head) {
    override def message: String = s"The name of the multi-function ${mf.name} is already taken by a global variable at" +
      s" ${variable.position}. Multi-functions and global variables may not share names."
  }

}
