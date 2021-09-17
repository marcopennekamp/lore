package lore.compiler.constraints

import lore.compiler.feedback.{MultiFunctionFeedback, Reporter}
import lore.compiler.semantics.{Core, Registry}

object CoreConstraints {

  /**
    * Verifies:
    *   1. All core multi-functions exist and have the correct parameter types and output type.
    *   2. TODO (modules): All core traits exist.
    */
  def verify()(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyCoreMultiFunctions()
  }

  private def verifyCoreMultiFunctions()(implicit registry: Registry, reporter: Reporter): Unit = {
    Core.multiFunctions.foreach { cmf =>
      registry.bindings.multiFunctions.get(cmf.name) match {
        case Some(mf) =>
          val min = mf.min(cmf.inputType)
          if (min.isEmpty) {
            reporter.error(MultiFunctionFeedback.Core.NotFound(cmf))
          } else {
            if (min.exists(_.signature.outputType </= cmf.outputType)) {
              reporter.error(MultiFunctionFeedback.Core.IllegalOutputType(cmf))
            }
          }
        case None => reporter.error(MultiFunctionFeedback.Core.NotFound(cmf))
      }
    }
  }

}
