package lore.compiler

import lore.compiler.types.DeclaredType
import lore.types.{AnyType, IntersectionType, Type}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable

/**
  * A hierarchy of declared types to provide quick access to supertype/subtype relationships between classes
  * and labels.
  */
class DeclaredTypeHierarchy {
  /**
    * The graph that holds the subtyping relationship. We start with Any type as the first node.
    */
  private val subtypingGraph: Graph[Type, DiEdge] = Graph(AnyType)
  private implicit val edgeFactory = DiEdge

  // TODO: Once class types can extend label types, we need to add these kinds of relationships to the hierarchy
  //       as well. If we declare an abstract function f for a label L, we want all classes explicitly declaring
  //       that they extend L to force the implementation of a specialized f for the given class type. This is
  //       essentially the same concept as requiring classes to implement interfaces in a single-dispatch world.

  /**
    * Adds the given type and potentially its supertype, if it isn't part of the hierarchy already. If the type to
    * add is already part of the hierarchy, this method does nothing.
    */
  def addType(declaredType: DeclaredType): Unit = {
    if (subtypingGraph.contains(declaredType)) {
      // We simply return silently as stated in the function description.
      return
    }

    // First we will want to add the supertype to the graph, provided it exists.
    declaredType.supertype.foreach(addType)

    // Then we can add the given type.
    subtypingGraph.addEdge(declaredType.supertype.getOrElse(AnyType), declaredType)

    // We should make sure that the graph is acyclic and connected.
    assert(subtypingGraph.isAcyclic)
    assert(subtypingGraph.isConnected)
  }

  /**
    * Asserts that the given type can be contained within the hierarchy. Only declared types and Any can be
    * contained in the hierarchy.
    */
  private def assertCanContain(tpe: Type): Unit = {
    assert(tpe == AnyType || tpe.isInstanceOf[DeclaredType])
  }

  /**
    * Returns all direct subtypes of the given type. `tpe` may either be Any or a declared type. At this point, we
    * have to assume that all declared types that could possibly be requested here have been registered with the
    * Registry and hence also added to this hierarchy. Thus, if the given type cannot be found in the hierarchy,
    * we throw a runtime exception with the assumption that it is a compiler bug, not a user error.
    */
  def getDirectSubtypes(tpe: Type): Set[DeclaredType] = {
    assertCanContain(tpe)
    if (!subtypingGraph.contains(tpe)) {
      throw new RuntimeException(s"The declared type hierarchy doesn't contain the type $tpe. This is a compiler bug.")
    }

    // Otherwise, we follow all the outbound edges and aggregate the declared types into a set.
    subtypingGraph.get(tpe).outgoing.map(_.to.value).map {
      case subtype: DeclaredType => subtype
      case subtype =>
        // Since only the root may be Any, any possible direct subtype should be a declared type.
        throw new RuntimeException(s"The type $subtype should be a declared type, as it's a subtype in a type hierarchy.")
    }
  }

  /**
    * Searches the graph breadth-first, but in the opposite direction, going towards predecessors.
    */
  private def reverseBfs(start: subtypingGraph.NodeT, visit: subtypingGraph.NodeT => Unit): Unit = {
    var remaining = start :: Nil
    while (remaining.nonEmpty) {
      val node = remaining.head
      remaining = remaining.tail
      visit(node)
      remaining = remaining ::: node.diPredecessors.toList
    }
  }

  /**
    * Returns the least common supertype of the two types, which is used by the algorithm that computes the least
    * upper bound of two arbitrary types.
    *
    * If these two types have multiple classes/labels as their least common ancestor, we return an intersection type
    * composed of all least common ancestors.
    */
  def leastCommonSupertype(t1: Type, t2: Type): Type = {
    assertCanContain(t1)
    assertCanContain(t2)

    // TODO: We probably don't need this marking scheme. Instead, we can add items to different sets.
    sealed trait Status
    case object Unseen extends Status    // A type not yet seen.
    case object Marked extends Status    // An ancestor of t1.
    case object Found extends Status     // One of the least common ancestors.
    case object Excluded extends Status  // A common ancestor, but not one of the least common ancestors.

    val status = mutable.HashMap[Type, Status]()

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

    // Set all Found ancestors of any Found node to Excluded.
    def getFoundTypes = status.toList.filter { case (_, status) => status == Found }.map(_._1)
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
}
