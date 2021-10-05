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
  def accessCoercion(position: Position): Binding => Option[Expression] = {
    case mf: MultiFunctionDefinition =>
      // Multi-functions which aren't directly used in a simple call must be converted to function values immediately.
      Some(Expression.MultiFunctionValue(mf, new InferenceVariable, position))

    // TODO (inference): To properly infer a struct constructor with bidirectional typechecking, we should introduce a
    //                   ConstructorValue expression, similar to the MultiFunctionValue expression.
    case structBinding: StructConstructorBinding => ??? //Some(Expression.BindingAccess(StructTransformation.getConstructor(structBinding, position), position))
    case binding: TypedBinding => Some(Expression.BindingAccess(binding, position))
  }
}
