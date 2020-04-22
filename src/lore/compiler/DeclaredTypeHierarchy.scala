package lore.compiler

import lore.types.{AnyType, DeclaredType, Type}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

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
    * Returns all direct subtypes of the given type. `tpe` may either be Any or a declared type. At this point, we
    * have to assume that all declared types that could possibly be requested here have been registered with the
    * Registry and hence also added to this hierarchy. Thus, if the given type cannot be found in the hierarchy,
    * we throw a runtime exception with the assumption that it is a compiler bug, not a user error.
    */
  def getDirectSubtypes(tpe: Type): Set[DeclaredType] = {
    assert(tpe == AnyType || tpe.isInstanceOf[DeclaredType])
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
}
