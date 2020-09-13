package lore.compiler.types

import lore.compiler.core.CompilationException
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable

/**
  * A hierarchy of declared and component types to provide quick access to supertype/subtype relationships between
  * structs, traits, and component types.
  */
class DeclaredTypeHierarchy {
  /**
    * The graph that holds the subtyping relationship. We start with Any as the first node.
    */
  private val subtypingGraph: Graph[Type, DiEdge] = Graph(BasicType.Any)
  private implicit val edgeFactory = DiEdge

  /**
    * Adds the given type and potentially its supertypes, if they aren't part of the hierarchy already. If the type to
    * add is already part of the hierarchy, this method does nothing.
    *
    * @param tpe Must either be a DeclaredType or a ComponentType.
    */
  def addType(tpe: Type): Unit = {
    if (subtypingGraph.contains(tpe)) return

    tpe match {
      case tpe: DeclaredType =>
        if (tpe.supertypes.nonEmpty) {
          tpe.supertypes.foreach { supertype =>
            addType(supertype)
            subtypingGraph.addEdge(supertype, tpe)
          }
        } else {
          subtypingGraph.addEdge(BasicType.Any, tpe)
        }
      case tpe: ComponentType =>
        subtypingGraph.addEdge(BasicType.Any, tpe)
      case _ =>
        throw CompilationException(s"Only declared types and component types may be added to the type hierarchy. Attempted to add type: $tpe.")
    }

    // We should make sure that the graph is still acyclic and connected.
    if (!subtypingGraph.isAcyclic) throw CompilationException(s"The type hierarchy is not acyclic anymore after adding type: $tpe.")
    if (!subtypingGraph.isConnected) throw CompilationException(s"The type hierarchy is not connected anymore after adding type: $tpe.")
  }

  /**
    * Asserts that the given type can be contained within the hierarchy. Only declared types, component types, and Any
    * can be contained in the hierarchy.
    */
  private def assertCanContain(tpe: Type): Unit = {
    if (tpe != BasicType.Any && !tpe.isInstanceOf[DeclaredType] && !tpe.isInstanceOf[ComponentType]) {
      throw CompilationException(s"Only declared types and component types are contained in the type hierarchy. Attempted to retrieve type: $tpe.")
    }
  }

  /**
    * Returns all direct subtypes of the given type. `tpe` may either be Any, a declared type, or a component type.
    * If the given type cannot be found in the hierarchy, it is assumed that it does not have any direct subtypes
    * (such as a component type that is not part of any struct or trait) and the empty set is returned.
    *
    * @param tpe Either Any, a declared type, or a component type.
    * @return A set of declared types and component types.
    */
  def getDirectSubtypes(tpe: Type): Set[Type] = {
    assertCanContain(tpe)
    if (subtypingGraph.contains(tpe)) {
      subtypingGraph.get(tpe).outgoing.map(_.to.value).map {
        // Since only the root may be Any, any possible direct subtype should be a declared type or a component type.
        case subtype: DeclaredType => subtype
        case subtype: ComponentType => subtype
        case subtype =>
          throw CompilationException(s"The type $subtype should be a declared type or a component type, as it's a subtype in a type hierarchy.")
      }
    } else Set.empty
  }

  /**
    * Returns the least common supertype of two types found in the type hierarchy. This result is used by the algorithm
    * that computes the least upper bound of two arbitrary types.
    *
    * If these two types have multiple traits as their least common ancestor, we return an intersection type
    * composed of all these ancestors.
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
}
