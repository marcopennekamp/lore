package lore.compiler.phases.transformation.inference

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.InferenceOrder.InfluenceGraph
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.ResolutionDirection
import lore.compiler.semantics.Registry
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object SimpleResolution {

  /**
    * Infers a set of assignments from the given assignments and judgments. First builds an influence graph from the
    * list of judgments, then resolves one resolvable judgment, consuming it in the process. This produces a new set
    * of assignments and a new judgment list, which is given to the next step recursively.
    *
    * This function might delegate to [[CycleResolution.infer]] if it cannot resolve judgments directly.
    *
    * The influence graph approach has the distinct advantage of being able to fail clearly and fast, compared to some
    * other methods such as fixed-point inference.
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

    println()
    influenceGraph.edges.foreach(println)
    println()

    judgments.firstDefined(judgment => attempt(assignments, influenceGraph, judgment, judgments.filter(_ != judgment))) match {
      case None =>
        // If no judgments have been resolved, simple resolution has failed. We need to fall back to cycle resolution.
        CycleResolution.infer(assignments, influenceGraph, judgments)

      case Some(compilation) => compilation.map(logIterationResult).flatMap((infer _).tupled)
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
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Option[Compilation[JudgmentResolver.Result]] = {
    def resolveTowards(direction: ResolutionDirection) = {
      println(s"Simple resolve $judgment")
      Some(JudgmentResolver.resolve(judgment, direction, assignments, influenceGraph, remainingJudgments))
    }

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

      case TypingJudgment.Fits(t1, _, _) =>
        if (isFullyInferred(t1, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.LeastUpperBound(_, types, _) =>
        // TODO: Add backwards direction once it's implemented in the judgment resolver.
        if (areFullyInferred(types, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.MemberAccess(_, source, _, _) =>
        // TODO: Add backwards direction once it's implemented in the judgment resolver.
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

      case TypingJudgment.MultiFunctionValue(target, _, _) =>
        if (isFullyInferred(target, assignments, influenceGraph)) {
          resolveTowards(ResolutionDirection.Forwards)
        } else None

      case TypingJudgment.MultiFunctionHint(_, _, _) => resolveTowards(ResolutionDirection.Backwards)
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
    InferenceBounds.areFixed(bounds) || !influenceGraph.find(iv).exists(_.hasPredecessors)
  }

  private def isFullyInferred(tpe: Type, assignments: Assignments, influenceGraph: InfluenceGraph): Boolean = {
    Inference.variables(tpe).forall(isFullyInferred(_, assignments, influenceGraph))
  }

  private def areFullyInferred(types: Vector[Type], assignments: Assignments, influenceGraph: InfluenceGraph): Boolean = {
    types.forall(isFullyInferred(_, assignments, influenceGraph))
  }

  def logIterationResult(result: JudgmentResolver.Result): JudgmentResolver.Result = {
    println("Iteration result assignments:")
    result._1.foreach(println)
    println()
    result
  }

}
