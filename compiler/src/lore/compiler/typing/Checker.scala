package lore.compiler.typing

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter, TypingFeedback, TypingFeedback2}
import lore.compiler.inference.Inference
import lore.compiler.inference.Inference.Assignments
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types._

/**
  * @param returnType The return type of the surrounding function, used to check `Return` expressions.
  */
case class Checker(returnType: Type) {

  private implicit val checker: Checker = this

  /**
    * Checks that `expression` has the type `expectedType` (or a subtype thereof). In the process, inference might
    * assign types to one or more inference variables, modeled via the input and output assignments.
    *
    * @param expectedType The type expected from the expression by the surrounding context. `expectedType` must be
    *                     fully instantiated.
    */
  def check(expression: Expression, expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Assignments = {
    // TODO (inference): This is a sanity check for now and can probably be removed once the algorithm is stable.
    if (!Inference.isFullyInstantiated(expectedType)) {
      throw CompilationException(s"The expected type $expectedType must be fully instantiated! Position: ${expression.position}.")
    }

    // The fallback is used when the expected type is clearly invalid in respect to the expression, for example when
    // the expression is a ListConstruction and the expected type is a tuple. We attempt inference as a fallback to
    // assign types to inference variables, so that the eventual subtyping error can be most informed. For example,
    // if we have a ListConstruction `[a.x, b.x]`, with `x` being a member of type `Int`, and an expected type
    // `(Int, Int)`, we want the resulting error to say "[Int] is not a subtype of (Int, Int)" instead of "[Any] is not
    // a subtype of (Int, Int)".
    // TODO (inference): If we allow e.g. lists to extend traits, this fallback is also instrumental in providing a
    //                   secondary path for type checking to accept a ListConstruction as a valid option for an
    //                   expected type `Enum`.
    def fallback: Assignments = Synthesizer.infer(expression, assignments)

    // Using `reportOnly` will suppress the default `SubtypeExpected` error in favor of an error that provides
    // additional context and information.
    var suppressDefaultError = false
    def reportOnly(error: Feedback.Error): Unit = {
      reporter.report(error)
      suppressDefaultError = true
    }

    // Step 1: Check and/or infer the expression's sub-expressions to produce an assignments map that will allow us to
    //         instantiate `expression.tpe`.
    val postAssignments = expression match {
      case Expression.Hole(_, _) => assignments // TODO (inference): Should we just ignore holes?

      case Expression.Return(value, _) =>
        check(value, returnType, assignments)

      case Expression.VariableDeclaration(_, value, typeAnnotation, _) =>
        // If a type annotation exists, we just have to check that the value has a compatible type. Otherwise, we need
        // to infer the type of the variable from the value expression.
        typeAnnotation match {
          case Some(typeAnnotation) => check(value, typeAnnotation, assignments)
          case None => fallback
        }

      case Expression.Assignment(target, value, _) =>
        val assignments2 = Synthesizer.infer(target, assignments)
        val targetType = Helpers.instantiate(target, assignments2)
        check(value, targetType, assignments2)

      case Expression.Tuple(values, _) =>
        // TODO (inference): One of the best things about bidirectional typechecking is that it can produce very
        //                   good errors. Instead of reporting `(a, b) is not a subtype of (a, b, c)`, we could
        //                   report `(a, b) is a tuple with 2 elements and thus cannot be a subtype of a tuple
        //                   (a, b, c) with 3 elements.`
        expectedType match {
          case TupleType(elements) if elements.length == values.length => check(values, elements, assignments)
          case _ => fallback
        }

      // TODO (inference): As it stands now, an anonymous function either requires an expected type context, or all of
      //                   its parameters to have type annotations. There is a very niche area where we could actually
      //                   infer the parameter's types based on their usage within the body. For example, a function
      //                   `x => x.name` could be typed as `%{ name: Any } => Any`. Of course that's not very useful if
      //                   we cannot also deduce the return type, but that's the gist of it. Supporting such a style of
      //                   inference would be, as said, very niche, so it's probably not worth the (considerable)
      //                   effort. However, we should still consider this down the line, when the new typechecking
      //                   algorithm is a bit more mature.
      case expression@Expression.AnonymousFunction(parameters, body, _) =>
        if (expression.isFullyAnnotated) {
          Synthesizer.infer(expression, assignments)
        } else {
          // If an anonymous function is missing a parameter type declaration, it requires the expected type to be a
          // function type.
          expectedType match {
            case expectedType@FunctionType(input, output) if input.elements.length == parameters.length =>
              Helpers.unifySubtypes(input, expression.tpe.input, assignments) match {
                case Some(assignments2) =>
                  check(body, output, assignments2)

                case None =>
                  val instantiatedInput = Helpers.instantiateCandidate(expression.tpe.input, assignments).asInstanceOf[TupleType]
                  reportOnly(TypingFeedback2.AnonymousFunctions.IllegalParameterTypes(expression, expectedType, instantiatedInput))
                  assignments
              }

            case expectedType: FunctionType =>
              reportOnly(TypingFeedback2.AnonymousFunctions.IllegalArity(expression, expectedType))
              assignments

            case _ =>
              reportOnly(TypingFeedback2.AnonymousFunctions.FunctionTypeExpected(expression, expectedType))
              assignments
          }
        }

      // TODO (inference): ConstructorValue.
      // TODO (inference): MultiFunctionValue.

      case Expression.ListConstruction(values, _) =>
        expectedType match {
          case ListType(elementType) => check(values, elementType, assignments)
          case _ => fallback
        }

      case Expression.MapConstruction(entries, _) =>
        expectedType match {
          case MapType(keyType, valueType) =>
            val assignments2 = check(entries.map(_.key), keyType, assignments)
            check(entries.map(_.value), valueType, assignments2)

          case _ => fallback
        }

      case Expression.ShapeValue(properties, _) =>
        expectedType match {
          case ShapeType(propertyTypes) =>
            properties.foldLeft(assignments) {
              case (assignments2, property) => propertyTypes.get(property.name) match {
                case Some(expectedProperty) => check(property.value, expectedProperty.tpe, assignments2)
                case None => Synthesizer.infer(property.value, assignments2)
              }
            }

          case _ => fallback
        }

      // TODO (inference): Call.

      case Expression.Cond(cases, _) =>
        val assignments2 = check(cases.map(_.condition), BasicType.Boolean, assignments)
        check(cases.map(_.body), expectedType, assignments2)

      case expression@Expression.WhileLoop(condition, _, _) =>
        val assignments2 = check(condition, BasicType.Boolean, assignments)
        checkLoop(expression, expectedType, assignments2)

      case expression@Expression.ForLoop(extractors, _, _) =>
        val assignments2 = Synthesizer.inferExtractors(extractors, assignments)
        checkLoop(expression, expectedType, assignments2)

      // The general case delegates to the Synthesizer, which simply infers the type of the expression. This
      // corresponds to a particular rule in most bidirectional type systems, defined as such:
      //    If `Γ ⊢ e => A` (infer) and `A = B` then `Γ ⊢ e <= B` (checked)
      // See: "Jana Dunfield and Neel Krishnaswami. 2020. Bidirectional Typing."
      // Of course, in Lore's type system, type equality must be replaced with subtyping, i.e. `A subtypes B`.
      case _ => Synthesizer.infer(expression, assignments)
    }

    // Step 2: Use the new assignments map to check that `expression.tpe` (as instantiated) is a subtype of
    //         `expectedType`, unless the default error has been suppressed by `reportOnly`.
    if (!suppressDefaultError) {
      val actualType = Helpers.instantiate(expression.tpe, postAssignments, expression)
      if (actualType </= expectedType) {
        // TODO (inference): Does this need a new typing error?
        reporter.error(TypingFeedback.SubtypeExpected(actualType, expectedType, expression))
      }
    }

    postAssignments
  }

  private def checkLoop(loop: Expression.Loop, expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Assignments = {
    expectedType match {
      case ListType(elementType) => check(loop.body, elementType, assignments)
      case _ => Synthesizer.infer(loop.body, assignments)
    }
  }

  /**
    * Executes [[check]] for all `expressions` in order.
    */
  def check(expressions: Vector[Expression], expectedType: Type, assignments: Assignments)(implicit reporter: Reporter): Assignments = {
    expressions.foldLeft(assignments) {
      case (assignments2, expression) => check(expression, expectedType, assignments2)
    }
  }

  /**
    * Executes [[check]] for all `expressions` and `exprectedTypes` pairs in order.
    */
  def check(expressions: Vector[Expression], expectedTypes: Vector[Type], assignments: Assignments)(implicit reporter: Reporter): Assignments = {
    expressions.zip(expectedTypes).foldLeft(assignments) {
      case (assignments2, (expression, expectedType)) => check(expression, expectedType, assignments2)
    }
  }

}
