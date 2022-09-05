package lore.compiler.typing

import lore.compiler.types.BasicType

package object unification {

  type InferenceAssignments = Map[InferenceVariable, InferenceBounds]

  implicit class InferenceAssignmentsExtension(assignments: InferenceAssignments) {
    /**
      * Returns the effective assignment of `iv`, which may or may not be contained in `assignments`.
      */
    def getEffective(iv: InferenceVariable): InferenceBounds = {
      assignments.getOrElse(iv, InferenceBounds(iv, BasicType.Nothing, BasicType.Any))
    }
  }

}
