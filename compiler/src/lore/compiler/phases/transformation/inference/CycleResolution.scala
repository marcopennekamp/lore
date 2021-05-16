package lore.compiler.phases.transformation.inference

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.InferenceOrder.InfluenceGraph
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.ResolutionDirection
import lore.compiler.semantics.Registry
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object CycleResolution {

  /**
    * When no judgments can be picked with fully inferred inference variables, we have a cycle in the influence graph.
    * Such a cycle has to be resolved by trial and error, with educated guesses about the pick.
    *
    * To pick an applicable judgment, we regard first the forwards direction and then the backwards direction of the
    * judgment. If the required source variables in the specific direction have no dependencies, we try to resolve the
    * judgment in that direction.
    *
    * TODO: Actually implement the "trial and recover" approach...
    *
    * TODO: If the direction fails, we should try the other direction.
    *
    * TODO: For now, the algorithm is greedy, always considering the first judgment that can be resolved in one or the
    *       other direction. We could also consider ALL applicable judgments in turn, recovering to the next one if
    *       compilation fails.
    *
    * TODO: Look at all judgments and see which are most desirable by assigning a sort of score. Choose the judgment
    *       with the best bounds score (i.e. both bounds set = 3, upper bound set = 2, lower bound set = 1; take the
    *       avg if multiple variables). Of course, the judgment must still be applicable, so the direction source may
    *       not have any external inbound edges (--> score = 0). The score should be calculated for both directions.
    *       The purpose of this is to avoid catastrophic inference degradation inferring Any or Nothing for every
    *       variable when starting at the wrong end. We should always strive to resolve the cycle with the most
    *       information available first.
    */
  def infer(assignments: Assignments, influenceGraph: InfluenceGraph, judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    judgments.firstDefined(judgment => isApplicable(judgment, influenceGraph).map((judgment, _))) match {
      case Some((judgment, direction)) =>
        println(s"Cycle resolve $judgment")
        JudgmentResolver.resolve(judgment, direction, assignments, judgments.filter(_ != judgment))
          .map(SimpleResolution.logIterationResult)
          .flatMap((SimpleResolution.infer _).tupled)

      case None =>
        throw CompilationException(
          "Type inference cannot further reduce the following list of judgments:\n" +
            judgments.mkString("\n") +
            "\nGiven the following list of assignments:\n" + // TODO: Print assignments in order.
            assignments.mkString("\n") +
            "\nAnd the following influence graph:\n" +
            influenceGraph.edges.mkString("\n") +
            "\n"
        )
    }
  }

  def isApplicable(judgment: TypingJudgment, influenceGraph: InfluenceGraph)(implicit registry: Registry): Option[ResolutionDirection] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      if (!hasExternalDependencies(t1, t2, influenceGraph) || !hasExternalDependencies(t2, t1, influenceGraph)) {
        Some(ResolutionDirection.Forwards)
      } else None

    case TypingJudgment.Subtypes(t1, t2, _) =>
      if (!hasExternalDependencies(t2, t1, influenceGraph)) {
        Some(ResolutionDirection.Forwards)
      } else if (!hasExternalDependencies(t1, t2, influenceGraph)) {
        Some(ResolutionDirection.Backwards)
      } else None

    case TypingJudgment.LeastUpperBound(target, types, _) =>
      // TODO: Add backwards direction once it's implemented in the judgment resolver.
      if (!hasExternalDependencies(types.flatMap(Inference.variables).toSet, Set(target), influenceGraph)) {
        Some(ResolutionDirection.Forwards)
      } else None

    case TypingJudgment.MemberAccess(target, source, _, _) =>
      // TODO: Add backwards direction once it's implemented in the judgment resolver.
      if (!hasExternalDependencies(source, target, influenceGraph)) {
        Some(ResolutionDirection.Forwards)
      } else None

    case TypingJudgment.MostSpecific(reference, alternatives, position) => ??? // TODO: Implement?

    case TypingJudgment.Conjunction(judgments, position) => ??? // TODO: Implement?

    case TypingJudgment.MultiFunctionHint(mf, arguments, position) =>
      // TODO: There can't even be cycles because the judgment has no internal inference variables, right?
      //       If so, a MultiFunctionHint can only be resolved during BulkResolution.
      //       This would fit with the notion that only bidirectional judgments can be taken into account during cycle
      //       resolution.
      None

    case _ =>
      // Other judgments like Assign can't have a cycle, because only one resolution direction is allowed.
      None
  }

  /**
    * Whether the source inference variables have external dependencies, meaning that there is at least one inbound
    * edge to a source variable that doesn't come from a target variable.
    *
    * TODO: This function may later also be used to calculate the score.
    */
  def hasExternalDependencies(ivs: Set[InferenceVariable], internalIvs: Set[InferenceVariable], influenceGraph: InfluenceGraph): Boolean = {
    ivs.toVector.map(iv => influenceGraph.get(iv)).exists {
      _.diPredecessors.map(_.value).exists {
        case iv: InferenceVariable => !internalIvs.contains(iv)
        case _ => true
      }
    }
  }

  def hasExternalDependencies(tpe: Type, internalType: Type, influenceGraph: InfluenceGraph): Boolean = {
    hasExternalDependencies(Inference.variables(tpe), Inference.variables(internalType), influenceGraph)
  }

}
