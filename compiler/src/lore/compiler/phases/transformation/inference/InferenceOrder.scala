package lore.compiler.phases.transformation.inference

import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.SetExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.immutable.Graph

object InferenceOrder {

  /**
    * The influence graph is built from a list of judgments. It models the dependencies of types and inference
    * variables to other types/inference variables.
    */
  type InfluenceGraph = Graph[Type, DiEdge]

  /**
    * A `source`/`target` pair where the `target` inference variable explicitly depends on the `source` type or
    * inference variable.
    */
  private case class Dependency(source: Type, target: InferenceVariable)

  private def dependenciesOf(judgment: TypingJudgment): Set[Dependency] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) => biDependenciesOf(t1, t2)
    case TypingJudgment.Subtypes(t1, t2, _) => biDependenciesOf(t1, t2)
    case TypingJudgment.Assign(target, source, _) => dependenciesOf(source, target)
    case TypingJudgment.Fits(t1, t2, _) => dependenciesOf(t1, t2)
    case TypingJudgment.LeastUpperBound(target, types, _) => biDependenciesOf(Vector(target), types)
    case TypingJudgment.MemberAccess(target, source, _, _) => biDependenciesOf(target, source)
    case operation: TypingJudgment.Operation => dependenciesOf(operation.operands, Vector(operation.target))
    case TypingJudgment.MultiFunctionValue(_, _, _) => Set.empty // TODO: Change this once MultiFunctionValues depend on the target.
    case hint@TypingJudgment.MultiFunctionHint(_, arguments, _) => dependenciesOf(Vector(hint.dependencyVariable), arguments)
  }

  private def dependenciesOf(sources: Set[Type], targets: Set[InferenceVariable]): Set[Dependency] = {
    targets.flatMap(target => sources.map(Dependency(_, target)))
  }

  /**
    * Find dependencies from all variables in `source` to all variables in `target`. If `source` contains no variables,
    * find all dependencies from `source` as a constant type to all variables in `target`.
    */
  private def dependenciesOf(source: Type, target: Type): Set[Dependency] = {
    dependenciesOf(
      Inference.variables(source).asInstanceOf[Set[Type]].ifEmptySingle(source),
      Inference.variables(target)
    )
  }

  /**
    * Find dependencies from all variables in `sources` to all variables in `targets`. If `sources` contain no
    * variables, find all dependencies from `sources` as constant types to all variables in `targets`.
    */
  private def dependenciesOf(sources: Vector[Type], targets: Vector[Type]): Set[Dependency] = {
    val sourceVariables = sources.flatMap(Inference.variables).toSet
    val targetVariables = targets.flatMap(Inference.variables).toSet
    dependenciesOf(sourceVariables.asInstanceOf[Set[Type]].ifEmpty(sources.toSet), targetVariables)
  }

  private def biDependenciesOf(t1: Type, t2: Type): Set[Dependency] = {
    dependenciesOf(t1, t2) ++ dependenciesOf(t2, t1)
  }

  private def biDependenciesOf(types1: Vector[Type], types2: Vector[Type]): Set[Dependency] = {
    dependenciesOf(types1, types2) ++ dependenciesOf(types2, types1)
  }

  def buildInfluenceGraph(judgments: Vector[TypingJudgment]): InfluenceGraph = {
    judgments.foldLeft(Graph(): InfluenceGraph) { case (graph, judgment) => addJudgment(graph, judgment) }
  }

  private def addJudgment(graph: InfluenceGraph, judgment: TypingJudgment): InfluenceGraph = {
    addDependencies(graph, dependenciesOf(judgment))
  }

  private def addDependencies(graph: InfluenceGraph, dependencies: Set[Dependency]): InfluenceGraph = {
    dependencies.foldLeft(graph) { case (graph2, dependency) =>
      graph2.incl(dependency.source ~> dependency.target)
    }
  }

}
