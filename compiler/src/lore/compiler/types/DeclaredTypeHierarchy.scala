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
    *
    * TODO: Abstract struct types can have different direct subtypes. A type `Some[A]` with some abstract `A` for
    *       example is abstract itself, because A is an open type argument. In such a case, given direct subtyping
    *       relationships `X < A` and `Y < A`, we should specialize `Some[A]` to `Some[X]` and `Some[Y]`.
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
    getDirectSubtypes(tpe).flatMap {
      subtype => if (subtype.isConcrete) Vector(subtype) else findConcreteSubtypes(subtype)
    }
  }

  /**
    * Returns the least common superschemas of two schemas found in the type hierarchy.
    */
  def leastCommonSuperschemas(s1: DeclaredSchema, s2: DeclaredSchema): Vector[NamedSchema] = {
    sealed trait Status
    case object Unseen extends Status    // A schema not yet seen.
    case object Marked extends Status    // An ancestor of s1.
    case object Found extends Status     // One of the least common ancestors.
    case object Excluded extends Status  // A common ancestor, but not one of the least common ancestors.

    val status = mutable.HashMap[NamedSchema, Status]()

    // Set all ancestors of s1 to Marked.
    reverseBfs(subtypingGraph.get(s1), node => {
      status.put(node.value, Marked)
    })

    // Set all Marked ancestors of s2 to Found.
    reverseBfs(subtypingGraph.get(s2), node => {
      val schema = node.value
      if (status.getOrElse(schema, Unseen) == Marked) {
        status.put(schema, Found)
      }
    })

    def getFoundSchemas = status.toVector.filter { case (_, status) => status == Found }.map(_._1)

    // Set all Found ancestors of any Found node to Excluded.
    getFoundSchemas.foreach { schema =>
      val node = subtypingGraph.get(schema)
      node.diPredecessors.foreach { predecessor =>
        if (status.contains(predecessor.value)) {
          status.put(predecessor.value, Excluded)
        }
      }
    }

    getFoundSchemas
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
