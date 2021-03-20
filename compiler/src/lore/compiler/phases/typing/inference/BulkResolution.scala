package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.phases.typing.inference.Inference.Assignments
import lore.compiler.phases.typing.inference.InferenceOrder.InfluenceGraph
import lore.compiler.phases.typing.inference.JudgmentResolver.ResolutionDirection
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object BulkResolution {

  /**
    * Infers a set of assignments from the given assignments and judgments. First builds an influence graph from the
    * list of judgments, then resolves all resolvable judgments, consuming them in the process. This produces a new set
    * of assignments and a shorter judgment list, which is given to the next step recursively.
    *
    * This function might delegate to [[CycleResolution.infer]] if it cannot resolve judgments directly.
    *
    * Because backtracking needs certain errors to be raised as soon as possible, the influence graph approach has the
    * distinct advantage of being able to fail clearly and fast (as opposed to a fixed-point approach). For example, a
    * faulty member access should always raise an error as soon as possible, which is harder to accomplish in
    * fixed-point type inference.
    */
  def infer(assignments: Assignments, judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    if (judgments.isEmpty) {
      return Compilation.succeed(assignments)
    }

    // TODO: Rebuilding the graph with every step is simple to code, but may be detrimental to performance. If this
    //       ever causes problems, we might want to consider removing fully inferred variables manually.
    //       As an alternative, we don't even NEED to build a graph, necessarily. There may be other kinds of
    //       representation to prepare the same information.
    val influenceGraph = InferenceOrder.buildInfluenceGraph(judgments)

    // Simultaneously build the new assignments and the list of remaining judgments.
    val compilation = judgments.foldLeft((assignments, Vector.empty[TypingJudgment]).compiled) { case (compilation, judgment) =>
      compilation.flatMap { case (assignments2, remainingJudgments) =>
        attempt(assignments2, influenceGraph, judgment) match {
          case Some(compilation2) => compilation2.map(assignments3 => (assignments3, remainingJudgments))
          case None => (assignments2, remainingJudgments :+ judgment).compiled
        }
      }
    }

    compilation.flatMap {
      case (newAssignments, remainingJudgments) =>
        // If no judgments have been consumed,
        if (judgments.length == remainingJudgments.length) {
          CycleResolution.infer(assignments, influenceGraph, judgments)
        } else {
          logIterationResult(newAssignments)
          infer(newAssignments, remainingJudgments)
        }
    }
  }

  /**
    * If the given judgment can be resolved (according to the assignments and influence graph), it is resolved,
    * resulting in new assignments. If the judgment cannot be resolved, the function returns None.
    */
  private def attempt(
    assignments: Inference.Assignments,
    influenceGraph: InfluenceGraph,
    judgment: TypingJudgment,
  )(implicit registry: Registry): Option[Compilation[Assignments]] = {
    def resolveTowards(direction: ResolutionDirection): Option[Compilation[Assignments]] = Some(JudgmentResolver.resolve(judgment, direction, assignments))

    judgment match {
      case TypingJudgment.Equals(t1, t2, _) =>
        if (isFullyInferred(t1, assignments, influenceGraph) || isFullyInferred(t2, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.Subtypes(t1, t2, _) =>
        if (isFullyInferred(t2, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else if (isFullyInferred(t1, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Backwards)
        } else None

      case TypingJudgment.Assign(_, source, _) =>
        if (isFullyInferred(source, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.LeastUpperBound(_, types, _) =>
        // TODO: Add backwards direction once it's implemented in `resolve`.
        if (areFullyInferred(types, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.MemberAccess(_, source, _, _) =>
        // TODO: Add backwards direction once it's implemented in `resolve`.
        if (isFullyInferred(source, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.ElementType(_, collection, _) =>
        if (isFullyInferred(collection, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.MultiFunctionCall(_, _, arguments, _) =>
        if (areFullyInferred(arguments, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.MostSpecific(_, _, _) => ???
    }
  }

  /**
    * An inference variable is fully inferred in two cases:
    *
    * 1. The variable's bounds are fixed, meaning that its lower and upper bounds are equal and can't change further.
    *    For the purpose of type inference, the inference variable can be used fully even if there are other judgments
    *    outstanding, since its bounds will never change and so it can even be seen as a type constant from then on.
    * 2. If the inference variable is a root of the current inference variable dependency graph, the variable cannot
    *    change further, because it has no "influences" left.
    */
  private def isFullyInferred(iv: InferenceVariable, assignments: Assignments, influenceGraph: InfluenceGraph): Boolean = {
    val bounds = InferenceVariable.effectiveBounds(assignments, iv)
    InferenceBounds.areFixed(bounds) || !influenceGraph.get(iv).hasPredecessors
  }

  private def isFullyInferred(tpe: Type, assignments: Assignments, influenceGraph: InfluenceGraph): Boolean = {
    // TODO: Does the constant inference variable extraction lead to performance issues?
    Inference.variables(tpe).forall(isFullyInferred(_, assignments, influenceGraph))
  }

  private def areFullyInferred(types: Vector[Type], assignments: Assignments, influenceGraph: InfluenceGraph): Boolean = {
    types.forall(isFullyInferred(_, assignments, influenceGraph))
  }

  def logIterationResult(assignments: Assignments): Unit = {
    println("Iteration result:")
    assignments.foreach(println)
    println()
  }

}
