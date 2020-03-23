package lore.functions

import lore.execution.Context
import scalaz.std.list._
import scalaz.syntax.traverse._
import lore.types.{Subtyping, TupleType}

object TotalityConstraint {

  /*
   * We have the following interesting case:
   *   Say we have types abstract X, A < X, B < X, C < X and a component +T. We have an abstract function with
   *   input X & +T that is implemented by a function with input (A | B) & +T, and a function with input C & +T.
   *   The totality constraint is checked such that the above situation is legal. This is implemented in the file
   *   abstract-sum-totality.lore.
   */

  /**
    * Verifies the multi-function for the totality constraint. In case of a violation, returns the set of functions
    * that are not fully implemented. If this set is empty, the multi-function satisfies the totality constraint.
    */
  def verify(mf: MultiFunction)(implicit context: Context): Set[LoreFunction] = {
    mf.functions.filter(_.isAbstract).filterNot(isVerified(mf))
  }

  /**
    * Verifies whether the given abstract function satisfies the totality constraint.
    */
  private def isVerified(mf: MultiFunction)(f: LoreFunction)(implicit context: Context): Boolean = {
    // We need to use the direct declared subtypes of the ABSTRACT parameters. An example will clear this up:
    //    Say we have types abstract A, A1 <: A, A2 <: A, non-abstract B, B1 <: B, B2 <: B.
    //    An abstract function f(a: A, b: B) with a non-abstract B must also cover the case f(a: AX, b: B), not just for
    //    B1 and B2, if the given value of type B is neither B1 nor B2, since it could just be B. Hence, we cannot use
    //    directDeclaredSubtypes, because it would substitute B1 and B2 for B, leaving B out of the equation entirely.
    f.inputType.abstractDirectDeclaredSubtypes.forall { subtype =>
      mf.functions.exists { f2 =>
        Subtyping.isStrictSubtype(f2.inputType, f.inputType) && mf.fit(subtype).contains(f2)
      }
    }
  }

}
