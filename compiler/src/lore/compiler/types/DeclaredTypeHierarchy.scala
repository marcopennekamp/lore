package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.definitions.TypeDefinition
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
class DeclaredTypeHierarchy(schemas: Iterable[TypeDefinition]) {

  /**
    * The graph that holds the subtyping relationship between declared type <i>schemas</i>. Any is the root node.
    */
  private val subtypingGraph: Graph[TypeDefinition, DiEdge] = {
    val edges = schemas.flatMap {
      case schema: DeclaredSchema =>
        schema.declaredSupertypes
          .map(t => t.schema ~> schema)
          .withDefault(BasicType.Any ~> schema)
      case _ => Vector.empty
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
    if (tpe.schema.isConstantSchema) concreteSubtypesCache.getOrElseUpdate(tpe, findConcreteSubtypes(tpe))
    else findConcreteSubtypes(tpe)
  }

  private def findConcreteSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    getDirectSubtypes(tpe).flatMap {
      subtype => if (subtype.isConcrete) Vector(subtype) else findConcreteSubtypes(subtype)
    }
  }

}
