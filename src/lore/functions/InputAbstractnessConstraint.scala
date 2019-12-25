package lore.functions

import lore.execution.Context
import lore.functions.TotalityConstraint.findDirectDeclaredSubtypes
import lore.types.Subtyping

object InputAbstractnessConstraint {

  /**
    * Checks whether the given multi-function satisfies the input abstractness constraint. Returns the set of functions
    * that don't satisfy the constraint. If the set is empty, the multi-function satisfies the constraint.
    */
  def verify(mf: MultiFunction)(implicit context: Context): Set[LoreFunction] = {
    mf.functions.filter(_.isAbstract).filterNot(_.inputType.isAbstract)
  }

}
