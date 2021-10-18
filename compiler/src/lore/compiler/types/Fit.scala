package lore.compiler.types

import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.unification.Unification

object Fit {

  /**
    * Whether t1 could be an argument type for a function with input type t2.
    *
    * Specifically, the following needs to be true for the fit to hold:
    *   1. ALL variables in t2 need to be assigned types from t1.
    *   2. All assignments to the same variable need to be the same type. We cannot permit subtypes in assignments.
    *      For example, assigning A = Real and then A = Int will fail, even though it would be quite valid in
    *      programming languages that don't resolve parametric types at run-time.
    *   3. Upper and lower bounds of all variables that are assigned to are made concrete. For example, for an
    *      allocation A = Real and B = Int and a bound B <= A, we get the bound B <= Real. Each type variable
    *      assignment is checked against its expected bounds. So for B = Int, we would check Int <= Real.
    *   4. Finally, we substitute all assigned variables in t2 with their assigned types. Let's call the result
    *      of this substitution s(t2). We need to check whether t1 is a subtype of s(t2).
    *
    * When all of these statements hold, t1 fits into t2. Note that when t2 is monomorphic, fit is equivalent to
    * subtyping, as checks (1), (2) and (3) trivially hold and (4) has no variables to substitute away, so the check
    * boils down to t1 <= t2 if t2 is monomorphic. (In such a case, t1 may either be polymorphic or monomorphic,
    * it doesn't matter.)
    */
  def fits(t1: Type, t2: Type): Boolean = {
    // Two types trivially fit into each other if they are equal.
    if (t1 == t2) return true

    // The type allocation in this.assignments handles (1), (2), and (3).
    assignments(t1, t2).exists { assignments =>
      val substituted = if (assignments.nonEmpty) Type.substitute(t2, assignments) else t2
      t1 <= substituted
    }
  }

  /**
    * Assigns types from `t1` to type variables in `t2`, returning the resulting type variable assignments. Otherwise
    * returns None if no consistent assignment can be found.
    */
  def assignments(t1: Type, t2: Type): Option[TypeVariable.Assignments] = {
    val typeVariables = Type.variables(t2).toVector
    val (s2, typeVariableAssignments) = InferenceVariable.fromTypeVariables(t2, typeVariables)
    Unification.unifyFits(t1, s2, Map.empty).flatMap { assignments =>
      Unification.unifyTypeVariableBounds(typeVariables, typeVariableAssignments, assignments).map { assignments2 =>
        typeVariableAssignments.map {
          case (tv, iv) => tv -> InferenceVariable.instantiateCandidate(iv, assignments2)
        }
      }
    }
  }

  /**
    * Whether t1 is more specific than t2.
    */
  def isMoreSpecific(t1: Type, t2: Type): Boolean = fits(t1, t2) && !fits(t2, t1)

  /**
    * Whether t1 is as specific as t2.
    */
  def isEquallySpecific(t1: Type, t2: Type): Boolean = fits(t1, t2) && fits(t2, t1)

}
