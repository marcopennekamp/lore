package lore.compiler.typing2

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
    * The objective of this function is to provide a base for [[lore.compiler.typing2.CallTyping]] (and potentially
    * other parts of typing or phases of compilation) to find which expressions can be inferred safely without
    * providing an external type context. Some expressions, such as the lambda in the call to `map_wrapper` in the test
    * `test/language/inference/wrapper.lore`, will be technically inferable without a type context, but the resulting
    * type will not be specific enough. This leads to problems during call typing: arguments are prematurely inferred
    * but then the call cannot be typed correctly. If such an argument was instead checked with the expected argument
    * type, the call could be typed correctly. [[isDefinitelyInferable]] can be used to avoid such premature inference.
    *
    * This needs to be kept in sync with the capabilities of [[lore.compiler.typing2.Checker2]].
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
    case call: UntypedValueCall => isDefinitelyInferable(call.target)
    case call: UntypedConstructorCall => call.target.isConstant || call.arguments.forall(isDefinitelyInferable)

    case access: UntypedBindingAccess => access.binding match {
      case _: UntypedLocalVariable | _: LocalVariable | _: GlobalVariableDefinition | _: StructObjectBinding => true
      case _ => false
    }

    case block: UntypedBlock if block.expressions.nonEmpty => isDefinitelyInferable(block.expressions.last)
    case cond: UntypedCond => cond.cases.forall(c => isDefinitelyInferable(c.body))
    case loop: UntypedLoop => isDefinitelyInferable(loop.body)

    case _ => true
  }
}
