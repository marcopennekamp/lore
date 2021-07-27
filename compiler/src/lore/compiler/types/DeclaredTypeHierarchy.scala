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
  * A hierarchy of declared types to provide quick access to supertype/subtype relationships between structs and traits.
  */
class DeclaredTypeHierarchy(declaredTypes: Vector[DeclaredType]) {

  private val concreteSubtypesCache = TrieMap[DeclaredType, Vector[DeclaredType]]()

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

    if (graph.isCyclic) throw CompilationException(s"The declared type hierarchy is cyclic. It must be acyclic.")
    if (!graph.isConnected) throw CompilationException(s"The declared type hierarchy is not connected. It must be connected.")

    graph
  }

  /**
    * Returns all direct subtypes of the given declared type, excluding the type itself.
    *
    * If the declared type `D` with schema `D*` has type arguments, each direct subtype `S` will be instantiated with
    * the most general type arguments. That is, for each type parameter `T` of `S`:
    *
    *   - If `T` occurs in `U` given the clause `extends D*[..., U, ...]`, we can find T's candidate type `C` based on
    *     a reverse substitution of `U`.
    *     - If `T` occurs multiple times in the type constructor `D[...]`, we have to combine each distinct occurrence
    *       based on T's variance to compute `C`:
    *       - Covariant: Take the intersection type of all candidates.
    *       - Contravariant: Take the sum type of all candidates.
    *       - Invariant: `S` cannot be a subtype of `D`, because `S` is invariant in `T` and cannot be instantiated
    *                    with two or more distinct types.
    *     - If T's bounds are narrower than `C`, we have three options based on T's variance:
    *       - Covariant: The most general type argument is T's upper bound.
    *         - Example: `Cage[Animal]` has the direct subtype `Aquarium[Fish]` (see `types.lore` test definitions).
    *       - Contravariant: The most general type argument is T's lower bound.
    *       - Invariant: `S` cannot be a subtype of `D`, because `S` is invariant in `T` and cannot be instantiated
    *                    with an assignment `T = C`.
    *   - Otherwise, we decide the most general instance of `T` based on its variance:
    *     - Covariant: The upper bound of `T`.
    *     - Contravariant: The lower bound of `T`.
    *     - Invariant: `T` itself, because the most general type argument must represent all possible type arguments,
    *                  and only a type variable can do that if `T` is invariant.
    *
    * Example: Consider types `trait A[X <: Animal, +Z <: Animal]` and `trait B[X, Y <: Animal, +Z <: Mammal] extends
    * A[X, Z]`. Assume we want to find the direct declared subtypes of `A[Bird, Animal]`. We can directly match `Bird`
    * to B's `X` and `Animal` to B's `Z`. However, because B's upper bound of `Z` is covariant and narrower than
    * `Animal`, we have `Z = Mammal`. `Y` cannot be derived from `A` and so we have to instantiate it based on variance
    * alone. As `Y` is invariant, we have no choice but to set `Y = Y`. Ultimately, the direct subtype this function
    * returns will be `B[Bird, Y, Mammal]`. This is the type that will be used to check whether the totality constraint
    * is satisfied for an abstract function.
    *
    * TODO (schemas): Actually implement type argument handling.
    */
  def getDirectSubtypes(tpe: NamedType): Vector[NamedType] = {
    // The resulting vector will contain distinct types, as the graph also contains distinct nodes.
    subtypingGraph.get(tpe).outgoing.toVector.map(_.to.value).filterType[DeclaredType]
  }

  /**
    * Returns all concrete subtypes of the given declared type, excluding the type itself. These may be direct or
    * indirect subtypes.
    *
    * The result of the operation is cached.
    */
  def getConcreteSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    concreteSubtypesCache.getOrElseUpdate(tpe, findConcreteSubtypes(tpe))
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
