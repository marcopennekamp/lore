package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.{Compilation, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.phases.transformation.inference.InferenceBounds.BoundType
import lore.compiler.phases.transformation.inference.InferenceOrder.InfluenceGraph
import lore.compiler.phases.transformation.inference._
import lore.compiler.semantics.Registry
import lore.compiler.types.{ProductType, Type}

object MultiFunctionHintJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionHint] {

  case class MultiFunctionHintMissingImplementation(judgment: TypingJudgment, successes: Int, compilations: Vector[Compilation[_]]) extends Error(judgment) {
    override def message: String = s"The MultiFunctionHint judgment $judgment cannot be resolved yet if there are $successes options. Sorry. Compilations:\n${compilations.mkString("\n")}."
  }

  /**
    * Our general approach here is to use typing information contained in each function definition to infer types for
    * the arguments. We're only consuming judgments that are needed to type the arguments. Each successful compilation
    * thus represents a possible set of argument types. A failed compilation means that the arguments don't agree with
    * the respective function definition.
    *
    * Failed compilations can be safely disregarded. However, if there are multiple sets of assignments produced by
    * multiple successful compilations, we will have to choose one set of assignments. This is where the
    * `resultArgumentType` comes into play: it can be used to choose the most specific argument type. If there is no
    * such most specific type, we have an ambiguity error. If there are no successful compilation at all, we have an
    * empty fit error.
    *
    * This resolver is not performing backtracking in the sense of building nested decision trees. Rather, the resolver
    * performs inference on a subset of judgments to produce a set of argument types which are then returned. The
    * resolution algorithm will simply continue to resolve judgments linearly. The advantage of the idea (as opposed to
    * backtracking) is that we can concentrate on the compilation successes, as all compilation errors will occur due
    * to argument typing errors. In a backtracking world, we would have had to decide between a compilation error that
    * occurred due to a falsely typed argument and a compilation error that happened down the line. In the former case,
    * we would have wanted to throw away the compilation, while the latter case must be reported to the user. A second
    * advantage is definitely performance, because inference can continue linearly instead of branching.
    */
  override def backwards(
    judgment: TypingJudgment.MultiFunctionHint,
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[(Assignments, Vector[TypingJudgment])] = {
    val TypingJudgment.MultiFunctionHint(mf, arguments, position) = judgment

    // Performance shortcut: If all inference variables are inferred to a point that they cannot change further, we can
    // skip the MultiFunctionHint, because it will provide no useful information.
    /* if (arguments.forall(argument => Inference.variables(argument).forall(v => assignments.get(v).exists(InferenceBounds.areFixed)))) {
      return Compilation.succeed((assignments, remainingJudgments))
    } */

    val inferredArgumentBounds = arguments.map { argument =>
      // This will always create bounds even if the inference variables of some arguments haven't been inferred yet,
      // in which case the bounds will be Nothing and/or Any.
      val lowerBound = instantiateByBound(assignments, argument, BoundType.Lower)
      val upperBound = instantiateByBound(assignments, argument, BoundType.Upper)
      (lowerBound, upperBound)
    }

    // TODO: Reevaluate whether the prefiltering based on types would still be beneficial. Now that we are only
    //       evaluating the argument types instead of all judgments in the nested step, the prefiltering might not save
    //       much computing time, if at all.
    // We can filter by arity immediately, because a function with a different arity will never be callable with
    // the given arguments. We can also filter based on already inferred argument bounds.
    val functions = mf.functions
      .filter(_.signature.arity == arguments.length)
      /*.filter(function => {
        function.signature.parameters.zip(inferredArgumentBounds).forall {
          case (parameter, (lowerArgumentBound, upperArgumentBound)) =>
            // Whether the argument type CANNOT fit the parameter type is determined by these exclusion criteria:
            //  - If the lower bound of the argument is a supertype of the upper bound of the parameter, the argument
            //    can NEVER fit. (See case arg3 below.)
            //  - If the upper bound of the argument is a subtype of the lower bound of the parameter, the argument
            //    can NEVER fit. (See case arg4 below.)
            // Example: Given C < B < A and foo(v: B)
            //  - B <= arg1 <= A  -->  arg1 could fit because arg1 could be B
            //  - C <= arg2 <= A  -->  arg2 could fit because arg2 could be B
            //  - A <= arg3 <= A  -->  arg3 can't fit because arg3 can never be B
            //  - C <= arg4 <= C  -->  arg4 can't fit because arg4 can never be B
            // TODO: This doesn't yet work if the argumentBound is a type variable. Most likely, we will have to
            //       substitute the type variable bounds into the argumentBound type as well.
            val lowerParameterBound = Type.substitute(parameter.tpe, _.lowerBound)
            val upperParameterBound = Type.substitute(parameter.tpe, _.upperBound)
            println(s"$lowerArgumentBound <= $upperParameterBound && $lowerParameterBound <= $upperArgumentBound")
            lowerArgumentBound <= upperParameterBound && lowerParameterBound <= upperArgumentBound
        }
      })*/

    val influencingJudgments = findInfluencingJudgments(arguments, assignments, influenceGraph, remainingJudgments)
    val resultArgumentType = new InferenceVariable

    val compilations = functions.map { function =>
      // Replace all type variables declared in the function with inference variables. The bounds relationships will
      // then be encoded as typing judgments between the arguments and the new inference variables. These inference
      // variables are only relevant for computing the argument types and need to be thrown away again afterwards.
      val typeVariables = function.typeScope.localTypeVariables
      val typeVariableAssignments = typeVariables.map(tv => (tv, new InferenceVariable)).toMap

      // TODO: When the bounds are fully instantiated types, we can simply change the assignments instead of adding new judgments.
      val lowerBoundsJudgments = typeVariables.flatMap { tv =>
        Some(TypingJudgment.Subtypes(Type.substitute(tv.lowerBound, typeVariableAssignments), typeVariableAssignments(tv), position))
      }

      // TODO: When the bounds are fully instantiated types, we can simply change the assignments instead of adding new judgments.
      val upperBoundsJudgments = typeVariables.flatMap { tv =>
        Some(TypingJudgment.Subtypes(typeVariableAssignments(tv), Type.substitute(tv.upperBound, typeVariableAssignments), position))
      }

      // For each argument/parameter pair, we add two typing judgments:
      //  - The Fits judgment ensures that any type variables on the parameter side (represented by inference
      //    variables) are properly assigned their bounds.
      //  - The Subtypes judgment allows inference of an argument's type based on the parameter type.
      val inputType = Type.substitute(function.signature.inputType, typeVariableAssignments).asInstanceOf[ProductType]
      val argumentJudgments = inputType.elements.zip(arguments).flatMap {
        case (parameterType, argument) => Vector(
          // TODO: Can't we pass the argument's position here?
          TypingJudgment.Fits(argument, parameterType, position),
          // TODO: We only really need the direction parameter --> argument. The other direction is already covered by
          //       the Fits judgment. Can we not restrict the Subtypes to a single direction here? This will likely
          //       reduce the "area of attack" for any bugs.
          TypingJudgment.Subtypes(argument, parameterType, position),
        )
      }

      val resultJudgments = Vector(
        TypingJudgment.Assign(resultArgumentType, ProductType(arguments), position)
      )

      val supplementalJudgments = lowerBoundsJudgments ++ upperBoundsJudgments ++ argumentJudgments ++ resultJudgments

      println("Hint judgments:")
      println((supplementalJudgments ++ influencingJudgments).mkString("\n"))

      SimpleResolution.infer(assignments, supplementalJudgments ++ influencingJudgments).map {
        assignments2 =>
          // We have to throw away the inference variables that only encode the function's type variables again, as
          // noted above.
          assignments2.removedAll(typeVariableAssignments.values)
      }
    }

    // TODO: Take the assignments that result in the MOST SPECIFIC function being chosen. If there is no such most
    //       specific function (either due to ambiguity or empty fit), return a compilation error.

    val successes = compilations.filter(_.isSuccess)
    if (successes.nonEmpty) {
      successes.simultaneous.map(_.distinct).flatMap { possibleAssignments =>
        if (possibleAssignments.length == 1) {
          Compilation.succeed((possibleAssignments.head, remainingJudgments.diff(influencingJudgments)))
        } else {
          Compilation.fail(MultiFunctionHintMissingImplementation(judgment, successes.length, compilations))
        }
      }
    } else {
      println(s"Empty fit of $judgment:")
      println(compilations.mkString("\n"))
      println()
      Compilation.fail(JudgmentResolver.EmptyFit(mf, ProductType(arguments))(judgment.position))
    }
  }

  /**
    * Find all typing judgments that still need to be resolved to fully type the arguments.
    *
    * We exclude inference bounds that are fixed from the dependencies, because we won't be able to change them. This
    * is, however, still an important step. For example, take the following code:
    *
    *     let values = [1, 2, 3]
    *     map(values, v => v + 5)
    *     map(values, v => v * 3)
    *
    * If we don't exclude the inference variable of `values`, the first map's MultiFunctionHint will INCLUDE the second
    * map's MultiFunctionHint. We want to avoid branching as much as possible.
    */
  private def findInfluencingJudgments(
    arguments: Vector[Type],
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment]
  ): Vector[TypingJudgment] = {
    val argumentInferenceVariables = arguments.flatMap(Inference.variables).toSet
    val dependencies = InferenceOrder.findDependencies(influenceGraph, argumentInferenceVariables) ++ argumentInferenceVariables
    val unfixedDependencies = dependencies.filterNot(iv => assignments.get(iv).exists(InferenceBounds.areFixed))
    InferenceOrder.findJudgmentsInfluencing(remainingJudgments, unfixedDependencies)
  }

}
