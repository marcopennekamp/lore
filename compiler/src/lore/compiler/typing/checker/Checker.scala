package lore.compiler.typing.checker

import lore.compiler.core.CompilationException
import lore.compiler.feedback._
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.synthesizer.{MultiFunctionValueSynthesizer, ParametricFunctionSynthesizer, Synthesizer}
import lore.compiler.typing.unification.Unification
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * @param returnType The return type of the surrounding function, used to check `Return` expressions.
  */
case class Checker(returnType: Type) {

  private implicit val checker: Checker = this

  /**
    * Checks that `expression` has the type `expectedType` (or a subtype thereof). In the process, inference might
    * assign types to one or more inference variables, modeled via the input and output assignments. Any typing errors
    * result in `None`.
    *
    * @param expectedType The type expected from the expression by the surrounding context. `expectedType` must be
    *                     fully instantiated.
    */
  def check(expression: Expression, expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Option[Assignments] = {
    if (!InferenceVariable.isFullyInstantiated(expectedType)) {
      throw CompilationException(s"The expected type $expectedType must be fully instantiated! Position: ${expression.position}. Assignments: $assignments")
    }

    // When the Checker can't handle an expression, we fall back to the Synthesizer. This corresponds to a particular
    // rule in most bidirectional type systems, defined as such:
    //    If `Γ ⊢ e => A` (infer) and `A = B` then `Γ ⊢ e <= B` (checked)
    // See: "Jana Dunfield and Neel Krishnaswami. 2020. Bidirectional Typing."
    // `fallback` is also used when the expected type is clearly invalid in respect to the expression, for example when
    // the expression is a ListConstruction and the expected type is a tuple. We attempt inference as a fallback to
    // assign types to inference variables, so that the eventual subtyping error can be most informed. For example,
    // if we have a ListConstruction `[a.x, b.x]`, with `x` being a member of type `Int`, and an expected type
    // `(Int, Int)`, we want the resulting error to say "[Int] is not a subtype of (Int, Int)" instead of "[Any] is not
    // a subtype of (Int, Int)".
    def fallback = Synthesizer.infer(expression, assignments)

    // Step 1: Check and/or infer the expression's sub-expressions to produce an assignments map that will allow us to
    //         instantiate `expression.tpe`.
    val resultAssignments: Option[Assignments] = expression match {
      case Expression.Hole(_, _) => Some(assignments)

      case Expression.Return(value, _) => check(value, returnType, assignments)

      case Expression.VariableDeclaration(variable, value, typeAnnotation, _) =>
        // If a type annotation exists, we just have to check that the value has a compatible type. Otherwise, we need
        // to infer the type of the variable from the value expression.
        typeAnnotation match {
          case Some(typeAnnotation) => check(value, typeAnnotation, assignments)
          case None => variable.tpe match {
            case iv: InferenceVariable =>
              Synthesizer.infer(value, assignments).flatMap { assignments2 =>
                val valueType = InferenceVariable.instantiateCandidate(value.tpe, assignments2)
                InferenceVariable.assign(iv, valueType, assignments2)
              }
            case _ => throw CompilationException(s"A variable declared without a type annotation should have an inference variable as its type. Position: ${expression.position}.")
          }
        }

      case Expression.Assignment(target, value, _) =>
        Synthesizer.infer(target, assignments).flatMap { assignments2 =>
          val targetType = InferenceVariable.instantiateCandidate(target, assignments2)
          check(value, targetType, assignments2)
        }

      case expression@Expression.Block(expressions, _, _) =>
        // If the expected type is Unit, we're relying on a feature that blocks have implicit unit values. After
        // typechecking, the type rehydration will additionally add these implicit unit values to the blocks that need
        // them.
        val effectiveExpectedType = if (expectedType == TupleType.UnitType) BasicType.Any else expectedType
        check(expressions.init, BasicType.Any, assignments)
          .flatMap(check(expressions.last, effectiveExpectedType, _))
          .flatMap(assignBlockType(expression, Some(expectedType), _))

      case expression@Expression.Tuple(values, _) =>
        expectedType match {
          case expectedType@TupleType(elements)  =>
            if (elements.length == values.length) {
              check(values, elements, assignments)
            } else {
              reporter.report(TypingFeedback.Tuples.IncorrectLength(expression, expectedType))
              None
            }

          case _ => fallback
        }

      case expression@Expression.AnonymousFunction(parameters, body, _) =>
        if (expression.isFullyAnnotated) {
          Synthesizer.infer(expression, assignments)
        } else {
          // If an anonymous function is missing a parameter type declaration, it requires the expected type to be a
          // function type.
          expectedType match {
            case expectedType@FunctionType(input, output) if input.elements.length == parameters.length =>
              Unification.unifySubtypes(input, expression.tpe.input, assignments) match {
                case Some(assignments2) =>
                  check(body, output, assignments2)

                case None =>
                  val instantiatedInput = InferenceVariable.instantiateCandidate(expression.tpe.input, assignments).asInstanceOf[TupleType]
                  reporter.error(TypingFeedback.AnonymousFunctions.IllegalParameterTypes(expression, expectedType, instantiatedInput))
                  None
              }

            case expectedType: FunctionType =>
              reporter.error(TypingFeedback.AnonymousFunctions.IllegalArity(expression, expectedType))
              None

            case _ =>
              reporter.error(TypingFeedback.AnonymousFunctions.FunctionTypeExpected(expression, expectedType))
              None
          }
        }

      case expression@Expression.MultiFunctionValue(mf, _, position) =>
        expectedType match {
          case expectedType@FunctionType(expectedInput, _) =>
            mf.dispatch(
              expectedInput,
              MultiFunctionFeedback.Dispatch.EmptyFit(mf, expectedInput, position),
              min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, expectedInput, min, position),
            ) match {
              case Some(instance) => MultiFunctionValueSynthesizer.handleFunctionInstance(instance, expression, Some(expectedType), assignments)
              case None =>
                // `dispatch` already reported an error.
                None
            }

          case BasicType.Any =>
            // If the expected type isn't a function type, but still can be a supertype of the function type, the
            // Synthesizer may be able to infer the multi-function value if the multi-function contains a single,
            // monomorphic function.
            fallback

          case _ =>
            reporter.error(TypingFeedback.MultiFunctionValues.FunctionTypeExpected(expression, expectedType))
            None
        }

      case Expression.UntypedConstructorValue(binding, tpe, _) =>
        expectedType match {
          case FunctionType(input, _) =>
            ParametricFunctionSynthesizer.inferTypeArguments(binding.signature, input.elements, assignments)
              .flatMap {
                case (typeParameterAssignments, assignments2) =>
                  InferenceVariable.assign(tpe, binding.asSchema.instantiate(typeParameterAssignments).constructorSignature.functionType, assignments2)
              }

          case _ => fallback
        }

      case Expression.ListConstruction(values, _) =>
        expectedType match {
          case ListType(elementType) => check(values, elementType, assignments)
          case _ => fallback
        }

      case Expression.MapConstruction(entries, _) =>
        expectedType match {
          case MapType(keyType, valueType) =>
            check(entries.map(_.key), keyType, assignments)
              .flatMap(check(entries.map(_.value), valueType, _))

          case _ => fallback
        }

      case Expression.ShapeValue(properties, _) =>
        expectedType match {
          case ShapeType(propertyTypes) =>
            properties.foldSome(assignments) {
              case (assignments2, property) => propertyTypes.get(property.name) match {
                case Some(expectedProperty) => check(property.value, expectedProperty.tpe, assignments2)
                case None => Synthesizer.infer(property.value, assignments2)
              }
            }

          case _ => fallback
        }

      case expression@Expression.Call(target, _, _, _) =>
        target match {
          case CallTarget.MultiFunction(mf) => MultiFunctionCallChecker.check(mf, expression, expectedType, assignments)

          case CallTarget.Constructor(structBinding) =>
            expectedType match {
              case dt: DeclaredType => ConstructorCallChecker.check(structBinding, expression, dt, assignments)
              case _ => fallback
            }

          case _ => fallback
        }

      case Expression.Cond(cases, _) =>
        check(cases.map(_.condition), BasicType.Boolean, assignments)
          .flatMap(check(cases.map(_.body), expectedType, _))

      case expression@Expression.WhileLoop(condition, _, _) =>
        check(condition, BasicType.Boolean, assignments)
          .flatMap(checkLoop(expression, expectedType, _))

      case expression@Expression.ForLoop(extractors, _, _) =>
        Synthesizer.inferExtractors(extractors, assignments)
          .flatMap(checkLoop(expression, expectedType, _))

      case _ => fallback
    }

    resultAssignments.flatMap { resultAssignments =>
      Typing.traceExpressionType(expression, resultAssignments, "Checked", s" (Expected type: $expectedType.)")

      // Step 2: Use the new assignments map to check that `expression.tpe` (as instantiated) is a subtype of
      //         `expectedType`.
      val actualType = InferenceVariable.instantiateCandidate(expression.tpe, resultAssignments)
      if (actualType </= expectedType) {
        reporter.error(TypingFeedback.SubtypeExpected(actualType, expectedType, expression))
        None
      } else {
        Some(resultAssignments)
      }
    }
  }

  def assignBlockType(
    block: Expression.Block,
    expectedType: Option[Type],
    assignments: Assignments,
  ): Option[Assignments] = {
    val lastExpressionType = InferenceVariable.instantiateCandidate(block.expressions.last, assignments)
    val resultType = expectedType match {
      case Some(TupleType.UnitType) => TupleType.UnitType
      case _ => lastExpressionType
    }
    InferenceVariable.assign(block.tpe.asInstanceOf[InferenceVariable], resultType, assignments)
  }

  private def checkLoop(loop: Expression.Loop, expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Option[Assignments] = {
    expectedType match {
      case ListType(elementType) => check(loop.body, elementType, assignments)
      case _ => Synthesizer.infer(loop.body, assignments)
    }
  }

  /**
    * Executes [[check]] for all `expressions` in order.
    */
  def check(expressions: Vector[Expression], expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Option[Assignments] = {
    expressions.foldSome(assignments) {
      case (assignments2, expression) => check(expression, expectedType, assignments2)
    }
  }

  /**
    * Executes [[check]] for all `expressions` and `expectedTypes` pairs in order.
    */
  def check(expressions: Vector[Expression], expectedTypes: Vector[Type], assignments: Assignments)(implicit reporter: Reporter): Option[Assignments] = {
    expressions.zip(expectedTypes).foldSome(assignments) {
      case (assignments2, (expression, expectedType)) => check(expression, expectedType, assignments2)
    }
  }

  /**
    * Attempts type checking via [[check]], using an internal reporter that accumulates errors, which are then returned
    * separately.
    *
    * `attempt` can be used to try a particular checking path without committing to it.
    */
  def attempt(expression: Expression, expectedType: Type, assignments: Assignments): (Option[Assignments], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    (check(expression, expectedType, assignments), reporter.feedback)
  }

}
