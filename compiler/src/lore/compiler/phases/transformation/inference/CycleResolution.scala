package lore.compiler.phases.transformation.inference

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, AssignmentsExtension}
import lore.compiler.phases.transformation.inference.InferenceOrder.InfluenceGraph
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.ResolutionDirection
import lore.compiler.semantics.Registry
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object CycleResolution {

  /**
    * When no judgments can be picked with fully inferred source inference variables, we have a cycle in the influence
    * graph. Such a cycle has to be resolved by picking an appropriate judgment.
    *
    * To pick such a judgment, we regard either the forwards or the backwards direction of the judgment. If the
    * required source variables in that direction have no external dependencies (meaning that only the judgment
    * itself will influence the inference variables), we try to resolve the judgment in that direction.
    *
    * Once picked, the choice is final. We do not recover from a compilation failure to pick another judgment or
    * direction. The reason is simple: We cannot differentiate between a compilation failure due to a faulty program
    * and a compilation failure due to a bad pick.
    */
  def infer(assignments: Assignments, influenceGraph: InfluenceGraph, judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[JudgmentResolver.Result] = {
    judgments.firstDefined(judgment => isApplicable(judgment, influenceGraph).map((judgment, _))) match {
      case Some((judgment, direction)) =>
        println(s"Cycle resolve $judgment")
        JudgmentResolver.resolve(judgment, direction, assignments, influenceGraph, judgments.filter(_ != judgment)).map(SimpleResolution.logIterationResult)

      case None =>
        throw CompilationException(
          "Type inference cannot further reduce the following list of judgments:\n" +
            judgments.mkString("\n") +
            "\nGiven the following list of assignments:\n" +
            assignments.stringified +
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

    case _ =>
      // Other judgments like Assign can't have a cycle, because only one resolution direction is allowed.
      None
  }

  /**
    * Whether the source inference variables have external dependencies, meaning that there is at least one inbound
    * edge to a source variable that doesn't come from a target variable.
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
