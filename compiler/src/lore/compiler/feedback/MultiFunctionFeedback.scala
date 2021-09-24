package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.functions.{CoreMultiFunction, FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.Type

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

  object Dispatch {
    case class EmptyFit(mf: MultiFunctionDefinition, inputType: Type, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot" +
        s" find a function of that name that would accept the given arguments with the type $inputType."
    }

    case class AmbiguousCall(mf: MultiFunctionDefinition, inputType: Type, min: Vector[FunctionDefinition], override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The multi-function call ${mf.name} at this site is ambiguous." +
        s" That is, we are finding too many functions that would accept the given arguments of type $inputType." +
        s" These are:\n${min.map("  - " + _.toString).mkString("\n")}"
    }

    case class FixedFunctionEmptyFit(mf: MultiFunctionDefinition, inputType: Type, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The multi-function ${mf.name} cannot be fixed with the argument types $inputType." +
        s" We cannot find a function that would accept arguments of the given types."
    }

    case class FixedFunctionAmbiguousCall(mf: MultiFunctionDefinition, inputType: Type, min: Vector[FunctionDefinition], override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The multi-function ${mf.name} cannot be fixed with the argument types $inputType." +
        s" We are finding too many functions that would accept arguments of the given types:\n" + min.map("  - " + _.toString).mkString("\n")
    }
  }
}
