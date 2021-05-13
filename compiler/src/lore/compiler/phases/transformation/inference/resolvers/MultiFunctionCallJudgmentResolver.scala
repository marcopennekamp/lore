package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.{Compilation, Position}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.transformation.inference.InferenceBounds.narrowBounds
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionInstance, MultiFunctionDefinition}
import lore.compiler.types.ProductType

object MultiFunctionCallJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionCall] {

  override def forwards(
    judgment: TypingJudgment.MultiFunctionCall,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // TODO: Once we activate the "MostSpecific" typing judgment that will also be generated with each multi-function
    //       call, we will already have performed the dispatch using the constraint solver. There might be no need to
    //       instantiate the function, for example, if we take the bounds inferred for the type variables, instead.
    // TODO: Handle upper and lower bounds separately.
    resolveDispatch(judgment.mf, ProductType(judgment.arguments), judgment.position, assignments).flatMap { instance =>
      val result = instance.signature.outputType
      narrowBounds(assignments, judgment.target, result, result, judgment)
    }
  }

  /**
    * TODO: Handle both upper and lower bounds separately.
    */
  def resolveDispatch(mf: MultiFunctionDefinition, uninstantiatedInputType: ProductType, position: Position, assignments: Inference.Assignments): Compilation[FunctionInstance] = {
    val inputType = instantiate(assignments, uninstantiatedInputType, _.candidateType).asInstanceOf[ProductType]
    mf.min(inputType) match {
      case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType)(position))
      case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min)(position))
      case functionDefinition +: _ => functionDefinition.instantiate(inputType)
    }
  }

}
