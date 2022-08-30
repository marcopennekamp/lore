package lore.compiler.typing2

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.core.CoreMultiFunction
import lore.compiler.semantics.expressions.Expression

object CoreBuilder {

  /**
    * Builds a multi-function call to a core multi-function such as `lore.core.to_string`.
    */
  def multiFunctionCall(
    cmf: CoreMultiFunction,
    arguments: Vector[Expression],
    position: Position,
  )(implicit reporter: Reporter): Option[Expression] = {
    cmf.mf.flatMap { mf =>
      // The specific instance's output type will be a subtype of the CMF's expected output type because the instance
      // is necessarily a specialization of the CMF. Output types are kept consistent by multi-function constraints.
      MultiFunctionTyping.buildMultiFunctionCall(mf, arguments, position)
    }
  }

}
