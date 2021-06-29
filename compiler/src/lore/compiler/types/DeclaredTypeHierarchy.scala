package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

import scala.collection.mutable

/**
  * A hierarchy of declared types to provide quick access to supertype/subtype relationships between structs and traits.
  */
class DeclaredTypeHierarchy(declaredTypes: Vector[DeclaredType]) {

  /**
    * The graph that holds the subtyping relationship. Any is the root node.
    */
  private val subtypingGraph: Graph[NamedType, DiEdge] = {
    val edges = declaredTypes.flatMap {
      tpe =>
        tpe.declaredSupertypes
          .asInstanceOf[Vector[NamedType]]
          .map(_ ~> tpe)
          .withDefault(BasicType.Any ~> tpe)
    }

    implicit val config: Graph.Config = CoreConfig()
    val graph = Graph.from(edges) + BasicType.Any

    if (graph.isCyclic) throw CompilationException(s"The declared type hierarchy must be acyclic, but is not.")
    if (!graph.isConnected) throw CompilationException(s"The declared type hierarchy must be connected, but is not.")

    graph
  }

  /**
    * Returns all direct subtypes of the given declared type. If the given type cannot be found in the hierarchy, it is
    * assumed that it does not have any direct subtypes and the empty list is returned.
    */
  def getDirectSubtypes(tpe: NamedType): Vector[NamedType] = {
    if (subtypingGraph.contains(tpe)) {
      // The resulting vector should contain only distinct elements. This is already given by the structure of the
      // type hierarchy, however. The given type will never be connected to the same subtype twice.
      subtypingGraph.get(tpe).outgoing.toVector.map(_.to.value).map {
        // Since only the root may be Any, any possible direct subtype should be a declared type.
        case subtype: DeclaredType => subtype
        case subtype =>
          throw CompilationException(s"The type $subtype should be a declared type, as it's a subtype in a declared type hierarchy.")
      }
    } else Vector.empty
  }

  /**
    * Returns the least common supertype of two types found in the type hierarchy. This result is used by the algorithm
    * that computes the least upper bound of two arbitrary types.
    *
    * If these two types have multiple traits as their least common ancestor, we return an intersection type composed
    * of all these ancestors.
    */
  def leastCommonSupertype(t1: DeclaredType, t2: DeclaredType): Type = {
    sealed trait Status
    case object Unseen extends Status    // A type not yet seen.
    case object Marked extends Status    // An ancestor of t1.
    case object Found extends Status     // One of the least common ancestors.
    case object Excluded extends Status  // A common ancestor, but not one of the least common ancestors.

    val status = mutable.HashMap[NamedType, Status]()

    // Set all ancestors of t1 to Marked.
    reverseBfs(subtypingGraph.get(t1), node => {
      status.put(node.value, Marked)
    })

    // Set all Marked ancestors of t2 to Found.
    reverseBfs(subtypingGraph.get(t2), node => {
      val tpe = node.value
      if (status.getOrElse(tpe, Unseen) == Marked) {
        status.put(tpe, Found)
      }
    })

    def getFoundTypes = status.toVector.filter { case (_, status) => status == Found }.map(_._1)

    // Set all Found ancestors of any Found node to Excluded.
    getFoundTypes.foreach { tpe =>
      val node = subtypingGraph.get(tpe)
      node.diPredecessors.foreach { predecessor =>
        if (status.contains(predecessor.value)) {
          status.put(predecessor.value, Excluded)
        }
      }
    }

    // The result is an intersection type of all found types.
    IntersectionType.construct(getFoundTypes)
  }

  /**
    * Searches the graph breadth-first, but in the opposite direction, going towards predecessors.
    */
  private def reverseBfs(start: subtypingGraph.NodeT, visit: subtypingGraph.NodeT => Unit): Unit = {
    var remaining = Vector(start)
    while (remaining.nonEmpty) {
      val node = remaining.head
      remaining = remaining.tail
      visit(node)
      remaining = remaining ++ node.diPredecessors.toVector
    }
  }

}
