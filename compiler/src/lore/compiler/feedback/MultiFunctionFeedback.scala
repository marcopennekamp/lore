package lore.compiler.feedback

import lore.compiler.semantics.functions.MultiFunctionDefinition

object MultiFunctionFeedback {
  case class NameTaken(mf: MultiFunctionDefinition) extends Feedback.Error(mf.functions.head) {
    override def message: String = s"The name of the multi-function ${mf.name} is already taken by a module or global" +
      s" variable. Modules, global variables, and multi-functions may not share names."
  }
}
