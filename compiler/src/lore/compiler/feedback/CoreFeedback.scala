package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.types.{TupleType, Type}

object CoreFeedback {
  object Trait {
    case class NotFound(name: NamePath) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The core trait $name is not defined. Please include Pyramid in your project" +
        s" dependencies or write your own trait definition."
    }

    case class TraitExpected(name: NamePath) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The type $name is not a trait. Please include Pyramid in your project" +
        s" dependencies or write your own proper trait definition."
    }
  }

  object MultiFunction {
    case class NotFound(name: NamePath, inputType: TupleType) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The core multi-function $name is not defined for the argument types $inputType." +
        s" Please include Pyramid in your project dependencies or write your own function definition."
    }

    case class IllegalOutputType(name: NamePath, inputType: TupleType, outputType: Type) extends Feedback.Error(Position.unknown) {
      override def message: String = s"The core multi-function $name for argument types $inputType has the wrong output" +
        s" type. Please include Pyramid in your project dependencies or ensure that the function has the following" +
        s" output type: $outputType."
    }
  }
}
