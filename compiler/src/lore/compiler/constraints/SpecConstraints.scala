package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.specs.SpecDefinition

object SpecConstraints {

  /**
    * Verifies:
    *   - Expression constraints for the spec's body.
    */
  def verify(spec: SpecDefinition)(implicit reporter: Reporter): Unit = {
    ExpressionConstraints.verify(spec.bodyNode)
  }

}
