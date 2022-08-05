package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, StructConstructorBinding, TermBinding, TypedTermBinding}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.typing.InferenceVariable

object BindingProcessors {
  /**
    * A term processor that coerces multi-functions and constructors to function values.
    */
  def accessCoercion(position: Position): TermBinding => Option[Expression] = {
    case mf: MultiFunctionDefinition =>
      // Multi-functions which aren't directly used in a simple call must be coerced to function values immediately.
      Some(Expression.MultiFunctionValue(mf, new InferenceVariable, position))

    case AmbiguousMultiFunction(multiReference) =>
      // Same as above, multi-functions not immediately called must be coerced to function values.
      // TODO (multi-import): Implement.
      ???

    case binding: StructConstructorBinding =>
      if (binding.isConstant) {
        Some(Expression.ConstructorValue(binding, binding.underlyingType, position))
      } else {
        Some(Expression.UntypedConstructorValue(binding, new InferenceVariable, position))
      }

    case binding: TypedTermBinding => Some(Expression.BindingAccess(binding, position))
  }
}
