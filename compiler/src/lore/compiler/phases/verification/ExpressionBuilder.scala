package lore.compiler.phases.verification

import lore.compiler.core.{Compilation, Position}
import lore.compiler.phases.verification.FunctionTransformationVisitor.{AmbiguousCall, EmptyFit}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.ProductType

object ExpressionBuilder {
  /**
    * Builds a simple multi-function call. Cannot be used to build constructor calls!
    */
  def multiFunctionCall(
    functionName: String, arguments: List[Expression], position: Position
  )(implicit registry: Registry): Compilation[Expression.Call] = {
    implicit val callPosition: Position = position
    registry.resolveMultiFunction(functionName).flatMap { mf =>
      val inputType = ProductType(arguments.map(_.tpe))
      mf.min(inputType) match {
        case Nil => Compilation.fail(EmptyFit(mf, inputType))
        case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min))
        case List(functionDefinition) =>
          functionDefinition.instantiate(inputType).map(instance => Expression.Call(instance, arguments, position))
      }
    }
  }
}
