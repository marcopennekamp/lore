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
    * @return The set of functions that are not fully implemented. If the set is empty, the multi-function satisfies
    *         the totality constraint.
    */
  def verify(mf: MultiFunction)(implicit context: Context): Set[LoreFunction] = {
    mf.functions.filter(_.isAbstract).filterNot(isVerified(mf))
  }

  /**
    * @return Whether the given abstract function satisfies the totality constraint.
    */
  private def isVerified(mf: MultiFunction)(f: LoreFunction)(implicit context: Context): Boolean = {
    findDirectDeclaredSubtypes(f.inputType).forall { subtype =>
      mf.functions.exists { f2 =>
        Subtyping.isStrictSubtype(f2.inputType, f.inputType) &&
          (!f2.isAbstract || (f2.isAbstract && isVerified(mf)(f2))) &&
          mf.fit(subtype).contains(f2)
      }
    }
  }

  private def findDirectDeclaredSubtypes(tupleType: TupleType)(implicit context: Context): Set[TupleType] = {
    tupleType.components
      .map(t => if (t.isAbstract) t.directDeclaredSubtypes.toList else List(t))
      .toList.sequence
      .map(types => TupleType(types))
      .toSet
  }

}
