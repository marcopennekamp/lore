package lore.compiler.semantics.functions

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

/**
  * A hierarchy of functions ordered according to their input types. Not necessarily connected, but definitely acyclic.
  */
case class DispatchHierarchy(graph: Graph[FunctionDefinition, DiEdge]) {

  /**
    * All root nodes in the hierarchy, which are functions without any corresponding superfunction.
    */
  lazy val roots: Vector[graph.NodeT] = graph.nodes.filter(_.inDegree == 0).toVector

}
