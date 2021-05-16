package lore.compiler.phases.transformation.inference

import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.immutable.Graph

object InferenceOrder {

  /**
    * The influence graph is built from a list of judgments. It models the dependencies of types and inference
    * variables to other types/inference variables.
    */
  type InfluenceGraph = Graph[Type, DiEdge]

  def buildInfluenceGraph(judgments: Vector[TypingJudgment]): InfluenceGraph = {
    judgments.foldLeft(Graph(): InfluenceGraph) { case (graph, judgment) => addJudgment(graph, judgment) }
  }

  private def addJudgment(graph: InfluenceGraph, judgment: TypingJudgment): InfluenceGraph = {
    judgment match {
      case TypingJudgment.Equals(t1, t2, _) => processBiDependencies(graph, t1, t2)
      case TypingJudgment.Subtypes(t1, t2, _) => processBiDependencies(graph, t1, t2)
      case TypingJudgment.Assign(target, source, _) => processDependencies(graph, source, target)
      case TypingJudgment.Fits(t1, t2, _) => processDependencies(graph, t1, t2)
      case TypingJudgment.LeastUpperBound(target, types, _) => processBiDependencies(graph, Vector(target), types)
      case TypingJudgment.MemberAccess(target, source, _, _) => processBiDependencies(graph, target, source)
      case operation: TypingJudgment.Operation => processDependencies(graph, operation.operands, operation.target)
      case TypingJudgment.MultiFunctionValue(_, _, _) =>
        // There is no need to add dependencies here, since `target` will get its dependencies from the context.
        graph
      case hint@TypingJudgment.MultiFunctionHint(_, arguments, _) => processDependencies(graph, hint.dependencyVariable, arguments)
    }
  }

  // TODO: Can we simplify the following helper functions?

  /**
    * Add dependency edges from all variables in `source` to all variables in `target`. If `source` contains no
    * variables, `source` is added to the influence graph as a constant type with edges to all variables in `target`.
    */
  private def processDependencies(graph: InfluenceGraph, source: Type, target: Type): InfluenceGraph = {
    val dependencies = Inference.variables(source).toVector.ifEmpty(source)
    val dependants = Inference.variables(target).toVector

    // Only add dependency edges if there are actually inference variables that depend on the given source.
    if (dependants.nonEmpty) {
      addDependencies(graph, dependencies, dependants)
    } else graph
  }

  private def processDependencies(graph: InfluenceGraph, sources: Vector[Type], target: Type): InfluenceGraph = {
    val sourceVariables = sources.flatMap(Inference.variables).distinct
    if (sourceVariables.nonEmpty) addDependencies(graph, sourceVariables, Vector(target))
    else sources.foldLeft(graph) { case (graph2, source) => processDependencies(graph2, source, target) }
  }

  private def processDependencies(graph: InfluenceGraph, source: Type, targets: Vector[Type]): InfluenceGraph = {
    val targetVariables = targets.flatMap(Inference.variables).distinct
    if (targetVariables.nonEmpty) addDependencies(graph, Vector(source), targetVariables)
    else graph
  }

  private def processBiDependencies(graph: InfluenceGraph, t1: Type, t2: Type): InfluenceGraph = {
    processDependencies(processDependencies(graph, t1, t2), t2, t1)
  }

  private def processBiDependencies(graph: InfluenceGraph, types1: Vector[Type], types2: Vector[Type]): InfluenceGraph = {
    // TODO: Can't simply fold. We have to prefer variables here, too... Otherwise we're producing useless type ~> iv
    //       edges.
    types1.foldLeft(graph) { case (graph2, t1) =>
      types2.foldLeft(graph2) { case (graph3, t2) =>
        processBiDependencies(graph3, t1, t2)
      }
    }
  }

  private def addDependencies(graph: InfluenceGraph, dependencies: Vector[Type], dependants: Vector[Type]): InfluenceGraph = {
    dependencies.foldLeft(graph) { case (graph2, dependency) =>
      dependants.foldLeft(graph2) { case (graph3, dependant) =>
        graph3.incl(dependency ~> dependant)
      }
    }
  }

}
