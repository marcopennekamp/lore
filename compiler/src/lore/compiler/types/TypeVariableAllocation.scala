package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.VectorExtension

import scala.collection.mutable

/**
  * Allows checking whether type variable assignments are consistent.
  *
  * An assignment is a single binding. An allocation is a nested set of assignments.
  */
class TypeVariableAllocation(variables: Set[TypeVariable]) {
  private val allocation = mutable.HashMap[TypeVariable, Vector[Type]]()

  /**
    * Adds the given type as an assignment of the type variable.
    */
  def addAssignment(tv: TypeVariable, tpe: Type): Unit = {
    val types = allocation.getOrElse(tv, Vector.empty)
    allocation.put(tv, types :+ tpe)
  }

  /**
    * All assignments in the current allocation. The allocation must be consistent, because otherwise we
    * can't return a single assigned type for each type variable.
    */
  lazy val assignments: TypeVariable.Assignments = {
    if (!isConsistent) throw CompilationException("The allocation must be consistent for assignments to be defined.")
    currentAssignments()
  }

  private def currentAssignments(): TypeVariable.Assignments = {
    allocation.view.mapValues {
      case representative +: _ => representative
      case _ => throw CompilationException("The allocation is invalid and thus doesn't define any consistent assignments.")
    }.toMap
  }

  /**
    * Whether these allocations are consistent. We essentially check these properties:
    *   1. All type variables are assigned a type.
    *   2. All types assigned to the same variable are equal.
    *   3. Assigned types are consistent with their variable's type bounds.
    */
  lazy val isConsistent: Boolean = allVariablesAssigned && areAssignmentsUnique && areBoundsKept

  /**
    * All type variables are assigned a type.
    */
  private def allVariablesAssigned: Boolean = (variables -- allocation.keySet).isEmpty

  /**
    * All types assigned to the same variable are equal.
    */
  private def areAssignmentsUnique: Boolean = allocation.values.forall(_.allEqual(identity))

  /**
    * Assigned types are consistent with their variable's type bounds.
    */
  private def areBoundsKept: Boolean = {
    val assignments = currentAssignments()
    assignments.forall { case (variable, tpe) =>
      val actualLowerBound = Type.substitute(variable.lowerBound, assignments)
      val actualUpperBound = Type.substitute(variable.upperBound, assignments)
      actualLowerBound <= tpe && tpe <= actualUpperBound
    }
  }

  override def toString: String = allocation.toString
}

object TypeVariableAllocation {

  /**
    * Creates a type variable allocation that represents all type variables in t2 which have been assigned types
    * from t1.
    *
    * TODO: Make this work for the following function:
    *           function foo(list: B) where A, B <: [A] = ...
    *       Currently, A cannot be inferred from the assigned type of B. However, we have the opportunity to infer
    *       it from the upper bound of B. In general: If we assign type t to B, we should also check whether we can
    *       assign t to B's upper bound (and lower bound). If the bounds contain variables, we will automatically
    *       "infer" the right type for A. Furthermore, this will result in a much needed type check for the
    *       following function:
    *           function foo(element: A, list: B) where A, B <: [A] = ...
    *       If we don't assign list's type to [A], we might not catch that element's A is not the same as B's A,
    *       i.e. when the given element and the given list have different (element) types.
    *       However, we have to be cautious. Consider the following function:
    *           function foo(a: A, b: B, c: C) where A, B <: A, C <: A = ...
    *       If we just blindly assign B's actual type to A (the upper bound) and do the same with C, we suddenly
    *       have an inconsistent assignment. For example: A = Int, B = Real, C = Real.
    *       So, we seem to arrive at a "forking" point for assignments:
    *         1. If a variable is mentioned in a parameter type, all instances of this variable must be equal.
    *         2. If a variable is mentioned in a bound of a variable, we have a subtyping relationship, not an
    *            equality relationship.
    *       We could collect both of these rules for all variables and then try to infer the type. If there is a
    *       proper assignment of the first kind, we only have to check that the other rules of the second kind
    *       hold as well with the given type. If there are only rules of the second kind, we can infer a variable
    *       assignment from the given subtyping rules, potentially using a LUB to resolve the issue.
    *
    * TODO: Considering that we want to infer variable assignments from upper and lower bounds, how can we still
    *       disallow the following?
    *           function genericListify(shape: { x: X, y: Y }): [R] where R, X <: R, Y <: R = [shape.x, shape.y]
    *           function genericListify(shape: { x: X, y: Y, z: Z }): [R] where R, X <: R, Y <: R, Z <: R = [shape.x, shape.y, shape.z]
    *       The problem with this multi-function is that when we pass a value of type { x: Real, y: Real } at compile-time
    *       and get a return type of [Real], we could easily pass a value of type { x: Real, y: Real, z: String } and
    *       get a return type of [Real | String] at run-time. This is clearly not valid!! So if we infer R = X | Y | Z,
    *       we will have to take care that the return type checks consider this, so that "[R = X | Y | Z] is not a
    *       subtype of [R = X | Y]" is the result of that constraint. NOT "[R] is equal to [R] so they are subtypes,
    *       clearly." That would be very bad. :)
    */
  def of(t1: Type, t2: Type): TypeVariableAllocation = {
    val allocation = new TypeVariableAllocation(Type.variables(t2))
    assign(t1, t2)(allocation)
    allocation
  }

  private def assign(t1: Type, t2: Type)(implicit allocation: TypeVariableAllocation): Unit = {
    // If the right-hand type contains no variables, there is no way we could assign anything, and thus the assignment
    // can be skipped. This check is currently important for correct compiler operation, as we only want to raise an
    // "unsupported substitution" error in cases where the right-hand type even contains type variables.
    if (Type.isMonomorphic(t2)) {
      return
    }

    def unsupportedSubstitution: Nothing = {
      throw CompilationException(s"Intersection and sum type type variable allocations are not yet supported." +
        s" Given types: $t1 and $t2.")
    }

    (t1, t2) match {
      case (_, tv2: TypeVariable) => allocation.addAssignment(tv2, t1)

      case (t1: TupleType, t2: TupleType) =>
        if (t1.elements.size == t2.elements.size) {
          t1.elements.zip(t2.elements).foreach { case (e1, e2) => assign(e1, e2) }
        }

      case (f1: FunctionType, f2: FunctionType) =>
        assign(f1.input, f2.input)
        assign(f1.output, f2.output)

      case (l1: ListType, l2: ListType) => assign(l1.element, l2.element)

      case (m1: MapType, m2: MapType) =>
        assign(m1.key, m2.key)
        assign(m1.value, m2.value)

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foreach {
          case (p2, Some(p1)) => assign(p1.tpe, p2.tpe)
          case _ =>
        }
      case (s1: StructType, s2: ShapeType) => assign(s1.asShapeType, s2)

      // Allocating types to intersection types and sum types is quite complex, since the allocation mechanism
      // suddenly comes upon more than one possible allocation. Take, for example, a sum type A | B, to which we
      // try to assign a type C. Should A or B become C? Surely not both A and B can be C (unless the sum type
      // is trivial). And even if we have a structurally similar type C | D, should A = C and B = D or A = D and
      // B = C? There are multiple possibilities.
      case (_: IntersectionType, _) => unsupportedSubstitution
      case (_, _: IntersectionType) => unsupportedSubstitution
      case (_: SumType, _) => unsupportedSubstitution
      case (_, _: SumType) => unsupportedSubstitution

      // If a declared type dt2 contains type variables in its type arguments, we have to assign them from the type
      // arguments of either dt1 itself or the supertypes of dt1. Crucially, to assign anything, dt1 and dt2 must have
      // the same schema, because type parameters have no direct relationship across different declared types.
      // This allocation is complicated by the fact that dt1 may contain multiple supertypes that directly or
      // indirectly lead to supertypes that have dt2's schema. For example:
      //    trait A[+X]
      //    trait B extends A[Animal]
      //    trait C extends A[Fish]
      //    trait D extends B, C
      // Let's say `dt1 = D` and `dt2 = A[X]`. We have two candidates from D: `A[Animal]` and `A[Fish]`. Because X is
      // covariant, we take the intersection type of these instances, namely `Animal & Fish = Fish`. If X wasn't
      // covariant, the allocation would be ambiguous and thus invalid.
      case (dt1: DeclaredType, dt2: DeclaredType) if dt2.schema.arity > 0 =>
        def collect(dt: DeclaredType): Vector[DeclaredType] = {
          if (dt.schema == dt2.schema) Vector(dt)
          else dt.declaredSupertypes.flatMap(collect)
        }

        val candidates = collect(dt1)
        if (candidates.nonEmpty) {
          for (i <- 0 until dt2.schema.arity) {
            val argument2 = dt2.typeArguments(i)
            if (candidates.length == 1) {
              assign(candidates.head.typeArguments(i), argument2)
            } else {
              val arguments1 = candidates.map(_.typeArguments(i))
              val parameter = dt2.schema.parameters(i)
              parameter.variance match {
                case Variance.Invariant => arguments1.foreach(assign(_, argument2))
                case Variance.Covariant => assign(IntersectionType.construct(arguments1), argument2)
                case Variance.Contravariant => assign(SumType.construct(arguments1), argument2)
              }
            }
          }
        }

      // In all other cases, there is no need to assign anything.
      case _ =>
    }
  }

}
