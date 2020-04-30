package lore.definitions

import lore.types.{Subtyping, Type}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable

case class MultiFunctionDefinition(name: String, functions: List[FunctionDefinition]) {
  /**
    * A hierarchy of functions ordered according to their input types. Not necessarily connected, but
    * definitely acyclic.
    */
  private val hierarchy: Graph[FunctionDefinition, DiEdge] = Graph()
  private implicit val edgeFactory = DiEdge
  buildHierarchy()

  /**
    * Returns the multi-function fit as defined in the specification.
    */
  def fit(t: Type): Set[FunctionDefinition] = {
    //functions.foreach(f => println(s"$t <= ${f.inputType}? ${Subtyping.isSubtype(t, f.inputType)}"))
    // TODO: Implement fit using the function hierarchy.
    functions.filter(f => Subtyping.isSubtype(t.toTuple, f.signature.inputType)).toSet
  }

  // TODO: Implement multiMin here, using the new function hierarchy?

  /**
    * Returns the function with the exact given input type.
    */
  def exact(inputType: Type): Option[FunctionDefinition] = {
    functions.find(f => f.signature.inputType == inputType.toTuple)
  }

  /**
    * Builds the function hierarchy that is used to define the fit.
    */
  private def buildHierarchy(): Unit = {
    // Algorithm: Sequentially pick all functions which don't have a super-function from the unused set and
    // add them to the graph. For all new functions f, add edges from any 0-out node f2 to f if in(f) < in(f2).
    // A 0-out node is a function which hasn't been associated with any sub-functions yet.
    // The algorithm connects functions whose input types are in direct subtyping relationships in the context of
    // all defined functions within this multi-function. It ensures this property by adding all functions which don't
    // have a super-function to the graph simultaneously. Such functions f must find their super-function g in at
    // least one of the graph's current 0-out nodes, because in the previous iteration, that f specifically wasn't
    // added to the graph because that super-function g was still part of the unused set.
    // Of course, this is not true for the first iteration. Then a function f cannot find a super-function g, but
    // rather becomes a root of the function hierarchy. Note that the graph can always contain functions which are
    // neither associated with sub-functions nor super-functions.
    val unused = mutable.HashSet[FunctionDefinition](functions: _*)
    while (unused.nonEmpty) {
      // All 0-out nodes.
      val zeros = hierarchy.nodes.filter(_.outDegree == 0)

      // All unused functions that don't have a super-function in the unused set.
      val supers = unused.toList.filterNot { f =>
        unused.filter(f2 => f2 != f).exists(f2 => f.signature.inputType <= f2.signature.inputType)
      }

      // Add these functions, then potentially connect them with any 0-out nodes.
      supers.foreach { f =>
        hierarchy.add(f)
        zeros.foreach { g =>
          if (f.signature.inputType <= g.signature.inputType) {
            hierarchy.addEdge(g, f)
          }
        }
      }

      // Finally, remove these functions from the unused set.
      supers.foreach(unused.remove)
    }
    assert(hierarchy.isAcyclic)
  }
}
