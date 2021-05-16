package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.{Compilation, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.phases.transformation.inference.InferenceBounds.BoundType
import lore.compiler.phases.transformation.inference.{Inference, InferenceBounds, InferenceVariable, SimpleResolution, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types.{ProductType, Type}

object MultiFunctionHintJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionHint] {

  case class MultiFunctionHintMissingImplementation(judgment: TypingJudgment, successes: Int, compilations: Vector[Compilation[_]]) extends Error(judgment) {
    override def message: String = s"The MultiFunctionHint judgment $judgment cannot be resolved yet if there are $successes options. Sorry. Compilations: $compilations."
  }

  override def backwards(
    judgment: TypingJudgment.MultiFunctionHint,
    assignments: Assignments,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[(Assignments, Vector[TypingJudgment])] = {
    val TypingJudgment.MultiFunctionHint(mf, arguments, position) = judgment

    // Performance shortcut: If all inference variables are inferred to a point that they cannot change further, we can
    // skip the MultiFunctionHint, because it will provide no useful information.
    if (arguments.flatMap(Inference.variables).forall(v => assignments.get(v).exists(InferenceBounds.areFixed))) {
      return Compilation.succeed((assignments, remainingJudgments))
    }

    val inferredArgumentBounds = arguments.map { argument =>
      // This will always create bounds even if the inference variables of some arguments haven't been inferred yet,
      // in which case the bounds will be Nothing and/or Any.
      val lowerBound = instantiateByBound(assignments, argument, BoundType.Lower)
      val upperBound = instantiateByBound(assignments, argument, BoundType.Upper)
      (lowerBound, upperBound)
    }

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

    println(s"Judgment: $judgment")
    println("All functions:")
    println(mf.functions)
    println("Filtered functions:")
    println(functions)
    println()

    // We can use this to determine the most specific argument types, which will be
    val resultArgumentType = new InferenceVariable

    val compilations = functions.map { function =>
      // Replace all type variables declared in the function with inference variables. The bounds relationships will
      // then be encoded as typing judgments between the arguments and the new inference variables.
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

      /* val resultJudgments = Vector(
        TypingJudgment.Equals(resultArgumentType, ProductType(arguments.map(_.tpe)), position)
      ) */

      val supplementalJudgments = lowerBoundsJudgments ++ upperBoundsJudgments ++ argumentJudgments

      println("Supplemental judgments:")
      println(supplementalJudgments)

      SimpleResolution.infer(assignments, supplementalJudgments ++ remainingJudgments).map {
        result => println(s"Chosen function: $function"); println(); result
      }
    }

    // TODO: Take the assignments that result in the MOST SPECIFIC function being chosen. If there is no such most
    //       specific function (either due to ambiguity or empty fit), return a compilation error.
    // TODO: We have to be careful with failed compilations. There are basically two cases:
    //        1. The compilation failed because an argument was typed incorrectly or because the multi-function call
    //           failed. In that case, we want to exclude the compilation from the "real" set.
    //        2. The compilation failed because of some other code error down the line, but the multi-function call
    //           went through correctly. We have to report this error to the user instead of suppressing it. So we have
    //           to somehow detect that and add it to the "real" set of compilation errors.
    //       In the end, we must not actually look at just the successes to determine which errors to pass to the user,
    //       but also at SOME of the failed compilations.
    //       This is all complicated by the fact that most likely, if the multi-function call is ambiguous, there will
    //       be no successes, but multiple compilation errors, some of which are the right errors to report.
    //       As for an approach how to solve this problem, I think we can look at the `resultArgumentType`. If this
    //       type is correctly set, the arguments have been correctly typed, which means that the compilation error is
    //       either due to an MF ambiguity/empty fit (which we want to report) or because of a compilation error down
    //       the line (which we also want to report). This of course requires that we get back the assignments from the
    //       nested inference even in the event of a compilation error...
    //       ANOTHER IDEA: Maybe we can only pass the supplementalJudgments to SimpleResolution.infer. However, that
    //       doesn't quite work. We also have to pass additional judgments that type the arguments further to the
    //       inference call. One judgment that comes to mind is a MultiFunctionValue judgment, which is definitely
    //       resolved AFTER the MultiFunctionHint. Deciding which to pass might be quite tricky. (It might be as easy
    //       as passing all judgments which the argument inference variables still depend on, but I haven't given this
    //       enough thought.) However, the advantage of this idea is that we can definitely concentrate on the
    //       compilation successes, as all compilation errors will be due to argument typing errors. This might even
    //       help with performance, because we're not performing the whole rest of inference inside the backtracking
    //       step.

    val successes = compilations.filter(_.isSuccess)
    if (successes.length == 1) {
      successes.head.map((_, Vector.empty))
    } else {
      println(successes)
      println()
      Compilation.fail(MultiFunctionHintMissingImplementation(judgment, successes.length, compilations))
    }
  }

}
