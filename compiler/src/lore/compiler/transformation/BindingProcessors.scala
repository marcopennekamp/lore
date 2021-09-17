package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.{Binding, StructConstructorBinding, TypedBinding}

object BindingProcessors {
  /**
    * A binding processor that coerces multi-functions and constructors to function values.
    */
  def accessCoercion(position: Position)(implicit judgmentCollector: JudgmentCollector): Binding => Option[Expression] = {
    case mf: MultiFunctionDefinition =>
      // Multi-functions which aren't directly used in a simple call must be converted to function values immediately.
      val functionType = new InferenceVariable
      judgmentCollector.add(TypingJudgment.MultiFunctionValue(functionType, mf, position))
      Some(Expression.MultiFunctionValue(mf, functionType, position))

    case structBinding: StructConstructorBinding => Some(Expression.BindingAccess(StructTransformation.getConstructor(structBinding, position), position))
    case binding: TypedBinding => Some(Expression.BindingAccess(binding, position))
  }
}
