package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * A hierarchy of declared schemas to provide quick access to hierarchical relationships between structs and traits.
  */
class DeclaredTypeHierarchy(schemas: Vector[DeclaredSchema]) {

  /**
    * The graph that holds the subtyping relationship between declared type <i>schemas</i>. Any is the root node.
    */
  private val subtypingGraph: Graph[NamedSchema, DiEdge] = {
    val edges = schemas.flatMap { schema =>
      schema.declaredSupertypes
        .map(t => t.schema ~> schema)
        .withDefault(BasicType.Any ~> schema)
    }

    implicit val config: Graph.Config = CoreConfig()
    val graph = Graph.from(edges) + BasicType.Any

    if (graph.isCyclic) throw CompilationException(s"The declared type hierarchy is cyclic. It must be acyclic.")
    if (!graph.isConnected) throw CompilationException(s"The declared type hierarchy is not connected. It must be connected.")

    graph
  }

  /**
    * Returns all direct subtypes of the given declared type, excluding the type itself. If a subtype schema has type
    * parameters, this function delegates to [[DeclaredType.specialize]] to instantiate a proper direct subtype.
    */
  def getDirectSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    // The resulting vector will contain distinct types, as the graph also contains distinct nodes.
    subtypingGraph.get(tpe.schema).outgoing.toVector
      .map(_.to.value)
      .filterType[DeclaredSchema]
      .flatMap(tpe.specialize)
  }

  private val concreteSubtypesCache = TrieMap[DeclaredType, Vector[DeclaredType]]()

  /**
    * Returns all concrete subtypes of the given declared type, excluding the type itself. These may be direct or
    * indirect subtypes.
    *
    * The result of the operation is cached for constant declared schemas.
    */
  def getConcreteSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    if (tpe.schema.isConstant) concreteSubtypesCache.getOrElseUpdate(tpe, findConcreteSubtypes(tpe))
    else findConcreteSubtypes(tpe)
  }

  private def findConcreteSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    // The .tail removes the node related to `tpe` from the results. This node always comes first.
    subtypingGraph.get(tpe)
      .outerNodeTraverser.toVector.tail
      .filterType[DeclaredType].filter(Type.isConcrete)
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
