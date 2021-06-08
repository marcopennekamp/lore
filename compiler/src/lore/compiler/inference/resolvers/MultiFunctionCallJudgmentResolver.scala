package lore.compiler.inference.resolvers

import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.DispatchFeedback.{AmbiguousCall, EmptyFit}
import lore.compiler.inference.Inference.{Assignments, instantiate}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionInstance, MultiFunctionDefinition}
import lore.compiler.types.ProductType

object MultiFunctionCallJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionCall] {

  override def forwards(
    judgment: TypingJudgment.MultiFunctionCall,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    resolveDispatch(judgment.mf, ProductType(judgment.arguments), judgment.position, assignments).flatMap { instance =>
      val result = instance.signature.outputType
      narrowBounds(assignments, judgment.target, result, judgment)
    }
  }

  def resolveDispatch(mf: MultiFunctionDefinition, uninstantiatedInputType: ProductType, position: Position, assignments: Inference.Assignments): Compilation[FunctionInstance] = {
    val inputType = instantiate(assignments, uninstantiatedInputType, _.candidateType).asInstanceOf[ProductType]
    mf.min(inputType) match {
      case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType, position))
      case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min, position))
      case functionDefinition +: _ => functionDefinition.instantiate(inputType)
    }
  }

}
