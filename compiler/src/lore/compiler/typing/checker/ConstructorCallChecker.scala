package lore.compiler.typing.checker

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.bindings.StructConstructorBinding
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{DeclaredType, StructType}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.Typing
import lore.compiler.typing.synthesizer.{ConstructorCallSynthesizer, ArgumentSynthesizer}
import lore.compiler.typing.unification.Unification
import lore.compiler.utils.CollectionExtensions.OptionExtension

object ConstructorCallChecker {

  /**
    * Checks the constructor call and infers its type parameter assignments given an already expected declared type as
    * a result. This expected type will inform the initial type arguments with which further arguments are inferred.
    *
    * Note that in the case of `binding` being a struct alias, we always infer the type arguments for the alias, not
    * for the underlying struct schema. The instantiated struct type's bounds are ensured by the definition of the
    * struct alias, which must define already compatible bounds for its own type parameters.
    */
  def check(
    binding: StructConstructorBinding,
    expression: Expression.Call,
    expectedType: DeclaredType,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    Typing.logger.trace(s"Checking of constructor call `${expression.position.truncatedCode}`:")
    Typing.indentationLogger.indented {
      val structSchema = binding.underlyingType.schema
      val expectedStructType = expectedType match {
        case structType: StructType if structType.schema == structSchema => structType
        case _ => expectedType.specialize(structSchema).filterType[StructType].getOrElse {
          reporter.error(TypingFeedback.ConstructorCall.CannotSpecialize(binding, expectedType, expression))
          return None
        }
      }

      val (typeParameterAssignments, parameterTypes) = ArgumentSynthesizer.prepareParameterTypes(binding.signature)

      // Given that we have an expected struct type, we must unify the expected constructor's parameters with the
      // actual parameter types so that any inference variables already known can be preassigned. For example, let's
      // say we have an expected struct type `Wrapper[Int, Any]` from the test case `language/inference/wrapper.lore`.
      // That is, we know the input type of the wrapper's function property. The expected constructor parameter is thus
      // typed as `Int => Any`. The unification takes care that the inference variable for `A` is assigned `Int` as an
      // upper bound.
      val expectedParameterTypes = expectedStructType.constructorSignature.parameters.map(_.tpe)
      val assignments2 = Unification.unifySubtypes(parameterTypes, expectedParameterTypes, assignments).getOrElse {
        return None
      }

      val (knownArgumentTypes, assignments3) = ArgumentSynthesizer.preprocessArguments(expression.arguments, assignments2)
      ConstructorCallSynthesizer.inferAndAssign(binding, expression, typeParameterAssignments, parameterTypes, knownArgumentTypes, assignments3)
    }
  }

}
