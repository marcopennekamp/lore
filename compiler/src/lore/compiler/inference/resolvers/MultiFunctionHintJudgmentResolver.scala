package lore.compiler.inference.resolvers

import lore.compiler.feedback.DispatchFeedback.EmptyFit
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.inference.Inference.{Assignments, isFullyInstantiated}
import lore.compiler.inference.InferenceOrder.InfluenceGraph
import lore.compiler.inference._
import lore.compiler.semantics.Registry
import lore.compiler.types.{BasicType, TupleType, Type}

object MultiFunctionHintJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionHint] {

  case class MultiFunctionHintMissingImplementation(judgment: TypingJudgment, successes: Int, results: Vector[Option[Assignments]]) extends Feedback.Error(judgment) {
    override def message: String = s"The MultiFunctionHint judgment $judgment cannot be resolved yet if there are $successes options. Sorry. Results:\n${results.mkString("\n")}."
  }

  /**
    * A multi-function hint judgment is resolved as follows:
    *
    * For each function definition with `n` parameters, a set of typing judgments is selected to infer the types of the
    * `n` arguments. These judgments consist of already existing judgments which the argument types depend on, and of
    * additional judgments created from parameter types.
    *
    * Each such set of judgments (each belonging to a function definition) is resolved with the inference algorithm,
    * resulting in a possible set of assignments of the argument types. If any error occurs, it means that for the
    * specific function definition, the arguments don't fit because already existing argument types and supposed
    * parameter types don't agree.
    *
    * Results with errors can be safely disregarded. However, if there are multiple different sets of assignments
    * produced by multiple successful paths, we will have to choose one set of assignments. This is where the
    * `resultArgumentType` comes into play: it's used to choose the most specific argument type (parallel to the fit of
    * a multi-function call). If there is no such most specific type, we have an ambiguity error. If there are no
    * successful paths at all, we have an empty fit error.
    *
    * This resolver is not performing backtracking in the sense of building nested decision trees. Rather, the resolver
    * performs inference on a subset of judgments to produce a set of argument types which are then returned. The
    * resolution algorithm will simply continue to resolve judgments linearly. The advantage of the idea (as opposed to
    * backtracking) is that we can concentrate on the successful paths, as all errors will occur due to argument typing
    * errors. In a backtracking world, we would have had to decide between an error that occurred due to a falsely
    * typed argument and an error that happened down the line. In the former case, we would have wanted to disregard
    * the path, while the latter case must be reported to the user. A second advantage is definitely performance,
    * because inference can continue linearly instead of branching.
    */
  override def backwards(
    judgment: TypingJudgment.MultiFunctionHint,
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
    val TypingJudgment.MultiFunctionHint(mf, arguments, position) = judgment
    val argumentTypes = judgment.argumentTypes

    // Performance shortcut: If all inference variables are inferred to a point that they cannot change further, we can
    // skip the MultiFunctionHint, because it will provide no useful information.
    if (arguments.forall(argument => Inference.variables(argument.tpe).forall(iv => InferenceVariable.isFixed(iv, assignments)))) {
      return Some((assignments, remainingJudgments))
    }

    // We can filter by arity immediately, because a function with a different arity will never be callable with
    // the given arguments.
    val functions = mf.functions.filter(_.signature.arity == arguments.length)

    val influencingJudgments = findInfluencingJudgments(argumentTypes, assignments, influenceGraph, remainingJudgments)
    val resultArgumentType = new InferenceVariable

    val results = functions.map { function =>
      // Replace all type variables declared in the function with inference variables. The bounds relationships will
      // then be encoded as typing judgments between the arguments and the new inference variables. These inference
      // variables are only relevant for computing the argument types and need to be thrown away again afterwards.
      val typeVariables = function.typeParameters
      val (typeVariableAssignments, boundsJudgments) = InferenceVariable.fromTypeVariables(typeVariables, position)

      // For each argument/parameter pair, we add two typing judgments:
      //  - The Fits judgment ensures that any type variables on the parameter side (represented by inference
      //    variables) are properly assigned their bounds.
      //  - The Subtypes judgment allows inference of an argument's type based on the parameter type.
      val argumentJudgments = function.signature.parameters.zip(arguments).flatMap {
        case (parameter, argument) =>
          val parameterType = Type.substitute(parameter.tpe, typeVariableAssignments)
          var judgments = Vector.empty[TypingJudgment]

          if (!isFullyInstantiated(parameterType)) {
            judgments = judgments :+ TypingJudgment.Fits(argument.tpe, parameterType, argument.position)
          }

          judgments :+ TypingJudgment.Subtypes(argument.tpe, parameterType, argument.position)
      }

      val resultJudgments = Vector(
        TypingJudgment.Assign(resultArgumentType, TupleType(argumentTypes), position)
      )

      val supplementalJudgments = boundsJudgments ++ argumentJudgments ++ resultJudgments
      val allJudgments = influencingJudgments ++ supplementalJudgments

      Inference.logger.trace(s"Multi-function hint judgments:\n${allJudgments.mkString("\n")}")

      val innerReporter: MemoReporter = MemoReporter()
      val assignments2 = SimpleResolution.infer(InferenceBounds.prefill(assignments, allJudgments), allJudgments)(registry, innerReporter)

      // We have to throw away the inference variables that only encode the function's type variables again, as
      // noted above.
      if (!innerReporter.hasErrors) Some(assignments2.removedAll(typeVariableAssignments.values))
      else None
    }

    // TODO: Take the assignments that result in the MOST SPECIFIC function being chosen. If there is no such most
    //       specific function (either due to ambiguity or empty fit), report an error and return None.

    val successes = results.flatten
    if (successes.nonEmpty) {
      val possibleAssignments = successes.distinct
      if (possibleAssignments.length == 1) {
        Some((possibleAssignments.head, remainingJudgments.diff(influencingJudgments)))
      } else {
        reporter.error(MultiFunctionHintMissingImplementation(judgment, successes.length, results))
        None
      }
    } else {
      Inference.logger.trace(s"Empty fit of `$judgment`:${results.mkString("\n")}")
      reporter.error(EmptyFit(mf, Inference.instantiateCandidateType(assignments, TupleType(argumentTypes)), judgment.position))
      None
    }
  }

  /**
    * Find all typing judgments that still need to be resolved to fully type the arguments.
    *
    * We exclude inference bounds that are fixed from the dependencies, because we won't be able to change them. For
    * example, take the following code:
    *
    *     let values = [1, 2, 3]
    *     map(values, v => v + 5)
    *     map(values, v => v * 3)
    *
    * If we don't exclude the inference variable of `values`, the first map's MultiFunctionHint will INCLUDE the second
    * map's MultiFunctionHint. We want to avoid nesting as much as possible.
    */
  private def findInfluencingJudgments(
    arguments: Vector[Type],
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment]
  ): Vector[TypingJudgment] = {
    val argumentInferenceVariables = arguments.flatMap(Inference.variables).toSet
    val dependencies = InferenceOrder.findDependencies(influenceGraph, argumentInferenceVariables) ++ argumentInferenceVariables
    val unfixedDependencies = dependencies.filterNot(InferenceVariable.isFixed(_, assignments))
    InferenceOrder.findJudgmentsInfluencing(remainingJudgments, unfixedDependencies)
  }

}
