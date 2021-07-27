package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.{MapExtension, VectorExtension}
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
    * parameters, this function delegates to [[instantiateDirectSubtypeSchema]] to instantiate a proper direct subtype.
    */
  def getDirectSubtypes(tpe: DeclaredType): Vector[DeclaredType] = {
    // The resulting vector will contain distinct types, as the graph also contains distinct nodes.
    subtypingGraph.get(tpe).outgoing.toVector.map(_.to.value).filterType[DeclaredSchema].flatMap { subtypeSchema =>
      if (subtypeSchema.isConstant) Some(subtypeSchema.representative)
      else instantiateDirectSubtypeSchema(tpe, subtypeSchema)
    }
  }

  /**
    * Instantiates a direct subtype schema with the most general type arguments, given a declared type.
    *
    * The general approach here is to consider the actual type arguments of `tpe`. We substitute them into all
    * occurrences of `tpe`'s schema that are contained in the `extends` clause of the subtype schema, which then allows
    * us to deduce all or part of the type arguments with which the subtype schema should be instantiated.
    *
    * Example: Consider types `trait A[X <: Animal, +Z <: Animal]` and `trait B[X, Y <: Animal, +Z <: Mammal] extends
    * A[X, Z]`. Assume we want to find the direct declared subtypes of `A[Bird, Animal]`. We can directly match `Bird`
    * to B's `X` and `Animal` to B's `Z`. However, because B's upper bound of `Z` is covariant and narrower than
    * `Animal`, we have `Z = Mammal`. `Y` cannot be derived from `A` and so we have to instantiate it based on variance
    * alone. As `Y` is invariant, we have no choice but to set `Y = Y`. Ultimately, the direct subtype this function
    * returns will be `B[Bird, Y, Mammal]`. This is the type that will be used to check whether the totality constraint
    * is satisfied for an abstract function.
    */
  private def instantiateDirectSubtypeSchema(tpe: DeclaredType, subtypeSchema: DeclaredSchema): Option[DeclaredType] = {
    val extendClauses = subtypeSchema.declaredSupertypes.filter(_.schema == tpe.schema)

    // Find all instantiations of the subtype schema's type parameters by substituting tpe's actual type arguments into
    // the type parameters of the subtype schema.
    val occurrences = extendClauses.foldLeft(Map.empty[TypeVariable, Vector[Type]]) {
      case (existingOccurrences, extendedType) =>
        val candidates = TypeVariableAllocation.of(TupleType(tpe.typeArguments), TupleType(extendedType.typeArguments)).allAssignments
        existingOccurrences.mergeWith(candidates)
    }.distinct

    // We have to keep track of the actual assignments from left to right so that we can instantiate a parameter's
    // lower and upper bound with the correct type arguments (if a bound depends on an earlier type parameter).
    val assignments = subtypeSchema.parameters.foldLeft(Map.empty: TypeVariable.Assignments) { case (assignments, parameter) =>
      val lowerBound = Type.substitute(parameter.lowerBound, assignments)
      val upperBound = Type.substitute(parameter.upperBound, assignments)

      val argument = occurrences.get(parameter) match {
        // Case 1: The subtype schema's type parameter occurs in at least one position and has received an assignment
        //         from one of tpe's type arguments.
        case Some(candidates) =>
          val candidate = if (candidates.length == 1) candidates.head else {
            // If there are multiple candidates, we have to combine them into a single assignment.
            parameter.variance match {
              case Variance.Covariant => IntersectionType.construct(candidates)
              case Variance.Contravariant => SumType.construct(candidates)
              case Variance.Invariant =>
                // Any type `S` instantiated via the subtype schema cannot be a subtype of `tpe``, because `S` is
                // invariant in the type parameter and cannot be instantiated with two or more distinct types.
                return None
            }
          }

          // If the parameter's bounds are narrower than the candidate, we have to fall back to one of the bounds, if
          // the type parameter's variance allows it.
          // Example: `Cage[Animal]` has the direct subtype `Aquarium[Fish]` (see `types.lore` test definitions),
          //          because `Aquarium` only accepts Fish.
          if (lowerBound </= candidate) {
            if (parameter.variance == Variance.Contravariant) lowerBound
            else return None
          } else if (candidate </= upperBound) {
            if (parameter.variance == Variance.Covariant) upperBound
            else return None
          } else {
            candidate
          }

        // Case 2: The subtype schema's type parameter does not occur in an `extends` position. We have to guess the
        //         most general type argument.
        case None => parameter.variance match {
          case Variance.Covariant => upperBound
          case Variance.Contravariant => lowerBound
          case Variance.Invariant =>
            // This must be the type parameter itself, because the most general type argument must represent all
            // possible type arguments, and only a type variable can do that if the parameter is invariant.
            // TODO (schemas): Really? Or would this be just Any?
            parameter
        }
      }

      assignments + (parameter -> argument)
    }

    subtypeSchema.instantiate(assignments).asInstanceOf[Option[DeclaredType]]
  }

  private val concreteSubtypesCache = TrieMap[DeclaredType, Vector[DeclaredType]]()

  /**
    * Returns all concrete subtypes of the given declared type, excluding the type itself. These may be direct or
    * indirect subtypes.
    *
    * The result of the operation is cached for declared schemas without type parameters.
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
