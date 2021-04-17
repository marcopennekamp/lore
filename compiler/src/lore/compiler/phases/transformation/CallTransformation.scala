package lore.compiler.phases.transformation

import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{CallTarget, FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.{ProductType, Type}

object CallTransformation {

  case class EmptyFit(mf: MultiFunctionDefinition, inputType: Type)(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot" +
      s" find a function of that name that would accept the given arguments with the type $inputType."
  }

  case class AmbiguousCall(
    mf: MultiFunctionDefinition,
    inputType: Type,
    min: Vector[FunctionDefinition],
  )(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site is ambiguous." +
      s" That is, we are finding too many functions that would accept the given arguments of type $inputType." +
      s" These are: ${min.mkString(", ")}."
  }

  /**
    * Builds a simple multi-function call without inference. Argument types may not contain inference variables.
    */
  def multiFunctionCall(
    functionName: String,
    arguments: Vector[Expression],
    position: Position,
  )(implicit registry: Registry): Compilation[Expression.Call] = {
    implicit val callPosition: Position = position
    registry.resolveMultiFunction(functionName).flatMap { mf =>
      val inputType = ProductType(arguments.map(_.tpe))
      mf.min(inputType) match {
        case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType))
        case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min))
        case functionDefinition +: _ =>
          functionDefinition.instantiate(inputType).map(
            instance => Expression.Call(CallTarget.MultiFunction(mf), arguments, instance.signature.outputType, position)
          )
      }
    }
  }

}