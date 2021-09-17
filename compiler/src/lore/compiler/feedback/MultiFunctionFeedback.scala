package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.functions.{CoreMultiFunction, MultiFunctionDefinition}

object MultiFunctionFeedback {
  case class NameTaken(mf: MultiFunctionDefinition) extends Feedback.Error(mf.functions.head) {
    override def message: String = s"The name of the multi-function ${mf.name} is already taken by a module or global" +
      s" variable. Modules, global variables, and multi-functions may not share names."
  }

  object Core {
    case class NotFound(cmf: CoreMultiFunction) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The core multi-function ${cmf.name} is not defined for the argument types" +
        s" ${cmf.inputType}. Please include Pyramid in your project dependencies or write your own function definition."
    }

    case class IllegalOutputType(cmf: CoreMultiFunction) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The core multi-function ${cmf.name} for argument types ${cmf.inputType} has the" +
        s" wrong output type. Please include Pyramid in your project dependencies or ensure that the function has the" +
        s" following output type: ${cmf.outputType}."
    }
  }
}
