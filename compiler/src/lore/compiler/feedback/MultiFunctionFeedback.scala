package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.types.{Type, TypeVariable}

object MultiFunctionFeedback {

  case class TypeParametersMissing(
    function: FunctionDefinition,
    missingTypeParameters: Vector[TypeVariable],
  ) extends Feedback.Error(function) {
    override def message: String = s"The function `${function.name}` declares type parameters which are not part of any" +
      s" parameter type. These are: ${missingTypeParameters.mkString(", ")}. Type parameters must occur in the type of" +
      s" at least one parameter."
  }

  case class DuplicateParameterName(signature: FunctionSignature, name: String) extends Feedback.Error(signature.position) {
    override def message: String = s"The function `${signature.name.simpleName}` has two or more parameters named" +
      s" `$name`. Parameter names must be unique."
  }

  case class DuplicateFunction(definition: FunctionDefinition) extends Feedback.Error(definition) {
    override def message: String = s"The function `${definition.signature}` is already declared somewhere else or has" +
      s" a type-theoretic duplicate."
  }

  case class IncompatibleOutputTypes(
    child: FunctionSignature,
    parent: FunctionSignature,
    parentInstance: FunctionSignature,
  ) extends Feedback.Error(child) {
    override def message: String = s"The function `$child` specializes `$parent`, but its output type ${child.outputType}" +
      s" is not a subtype of the parent's (instantiated) output type ${parentInstance.outputType}."
  }

  object Abstract {
    case class IllegallyAbstract(function: FunctionDefinition) extends Feedback.Error(function) {
      override def message: String = s"The function `${function.signature}` is declared abstract even though it doesn't" +
        s" have an abstract input type. Either implement the function or ensure the input type is abstract."
    }

    case class NotTotal(function: FunctionDefinition, missing: Vector[Type]) extends Feedback.Error(function) {
      override def message: String = s"The abstract function `${function.signature}` is not fully implemented and thus" +
        s" doesn't satisfy the totality constraint. Please implement functions for the following input types:" +
        s" ${missing.mkString(", ")}."
    }
  }

  object Dispatch {
    case class EmptyFit(
      mf: MultiFunctionDefinition,
      inputType: Type,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The multi-function call `${mf.name}` at this site has an empty fit. We cannot" +
        s" find a function of that name that would accept the given arguments with the type $inputType."
    }

    case class AmbiguousCall(
      mf: MultiFunctionDefinition,
      inputType: Type,
      min: Vector[FunctionDefinition],
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The multi-function call `${mf.name}` at this site is ambiguous." +
        s" That is, we are finding too many functions that would accept the given arguments of type $inputType." +
        s" These are:\n${min.map("  - " + _.toString).mkString("\n")}"
    }
  }

  object FixedFunction {
    case class EmptyFit(
      mf: MultiFunctionDefinition,
      inputType: Type,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The multi-function `${mf.name}` cannot be fixed with the argument types" +
        s" `$inputType`. We cannot find a function that would accept arguments of the given types."
    }

    case class AmbiguousCall(
      mf: MultiFunctionDefinition,
      inputType: Type,
      min: Vector[FunctionDefinition],
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The multi-function `${mf.name}` cannot be fixed with the argument types" +
        s" `$inputType`. We are finding too many functions that would accept arguments of the given types:\n" +
        min.map("  - " + _.toString).mkString("\n")
    }
  }

}
