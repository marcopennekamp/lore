package lore.compiler.phases.transformation.inference

import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.SetExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

object InferenceOrder {

  /**
    * The influence graph is built from a list of judgments. It models the dependencies of types and inference
    * variables to other types/inference variables.
    */
  type InfluenceGraph = Graph[Type, DiEdge]

  def buildInfluenceGraph(judgments: Vector[TypingJudgment]): InfluenceGraph = {
    val dependencies = judgments.flatMap(_.dependencies).toSet
    implicit val config: Graph.Config = CoreConfig()
    Graph.from(dependencies.map(dependency => dependency.source ~> dependency.target))
  }

  /**
    * Find all inference variables that the given inference variables depend on.
    */
  def findDependencies(graph: InfluenceGraph, ivs: Set[InferenceVariable]): Set[InferenceVariable] = {
    graph.nodes
      .filter(node => node.findSuccessor(successor => successor.value match {
        case iv: InferenceVariable => ivs.contains(iv)
        case _ => false
      }).isDefined)
      .map(_.value)
      .toSet
      .filterType[InferenceVariable]
  }

  /**
    * Find all typing judgments from the given list that may influence the bounds of one of the given inference
    * variables.
    */
  def findJudgmentsInfluencing(judgments: Vector[TypingJudgment], ivs: Set[InferenceVariable]): Vector[TypingJudgment] = {
    judgments.filter(judgment => judgment.dependencies.exists(dependency => ivs.contains(dependency.target)))
  }

}
