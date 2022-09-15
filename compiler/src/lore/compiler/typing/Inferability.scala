package lore.compiler.typing

import lore.compiler.semantics.bindings.{LocalVariable, StructObjectBinding, UntypedLocalVariable}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.semantics.variables.GlobalVariableDefinition

object Inferability {
  /**
    * Whether the given untyped expression can be inferred without external type information and without taking a guess
    * at type parameters. Local variables are assumed to be fully typed, which is a guarantee given by the current
    * inference algorithm. This allows [[isDefinitelyInferable]] to assume that all local variables are definitely
    * inferable, which covers a lot of the most common cases.
    *
    * The objective of this function is to provide a base for [[lore.compiler.typing.CallTyping]] (and potentially
    * other parts of typing or phases of compilation) to find which expressions can be inferred safely without
    * providing an external type context. Some expressions, such as the lambda in the call to `map_wrapper` in the test
    * `test/language/inference/wrapper.lore`, will be technically inferable without a type context, but the resulting
    * type will not be specific enough. This leads to problems during call typing: arguments are prematurely inferred
    * but then the call cannot be typed correctly. If such an argument was instead checked with the expected argument
    * type, the call could be typed correctly. [[isDefinitelyInferable]] can be used to avoid such premature inference.
    *
    * This needs to be kept in sync with the capabilities of [[lore.compiler.typing.Checker]].
    */
  def isDefinitelyInferable(expression: UntypedExpression): Boolean = expression match {
    case UntypedTupleValue(elements, _) => elements.forall(isDefinitelyInferable)

    case expression: UntypedLambdaValue =>
      // Fully annotated parameters don't suffice for the lambda value to be definitely inferable, because an expected
      // output type can still influence the resulting type of the lambda value (e.g. when the lambda itself returns a
      // parameterized struct).
      expression.isFullyAnnotated && isDefinitelyInferable(expression.body)

    case _: UntypedConstructorValue =>
      // This is a constructor value with explicitly specified type arguments, which is already fully typed.
      true

    case UntypedListValue(elements, _) => elements.forall(isDefinitelyInferable)
    case UntypedShapeValue(properties, _) => properties.forall(p => isDefinitelyInferable(p.value))

    case call: UntypedMultiFunctionCall => call.arguments.forall(isDefinitelyInferable)
    case call: UntypedAmbiguousMultiFunctionCall => call.arguments.forall(isDefinitelyInferable)
    case call: UntypedConstructorCall => call.target.isConstant || call.arguments.forall(isDefinitelyInferable)

    case UntypedValueCall(access: UntypedMemberAccess, arguments, _) =>
      // A value call `instance.function(a1, a2, ...)` on a member access might be a UCS call. Such a call must be
      // treated like it COULD be a multi-function call with `instance` as the first argument.
      isDefinitelyInferable(access) && arguments.forall(isDefinitelyInferable)

    case call: UntypedValueCall => isDefinitelyInferable(call.target)

    case access: UntypedBindingAccess => access.binding match {
      case _: UntypedLocalVariable | _: LocalVariable | _: GlobalVariableDefinition | _: StructObjectBinding => true
      case _ => false
    }

    case access: UntypedMemberAccess =>
      // A member access `instance.function` might be a function call `function(instance)` via the uniform call syntax.
      // Hence, the access should be treated like it COULD be a multi-function call with `instance` as its sole
      // argument, unless there is no available UCS binding. A non-UCS member access is always definitely inferable,
      // because the `instance` is not dependent on an expected type.
      access.ucsBinding.isEmpty || isDefinitelyInferable(access.instance)

    case block: UntypedBlock if block.expressions.nonEmpty => isDefinitelyInferable(block.expressions.last)
    case cond: UntypedCond => cond.cases.forall(c => isDefinitelyInferable(c.body))
    case loop: UntypedLoop => isDefinitelyInferable(loop.body)

    case _ => true
  }
}
