package lore.compiler.resolution.initialization

import lore.compiler.feedback.{AliasFeedback, SchemaFeedback, TypingFeedback}
import lore.compiler.test.BaseSpec

class DefinitionInitializationSpec extends BaseSpec {

  private val fragmentBase = "resolution/initialization"

  s"$fragmentBase/fallbacks" should "compile without crashing and report errors" in {
    // This ensures that fallback-initialization of cyclic type definitions and struct aliases without an underlying
    // struct type is carried out correctly. Especially, a compiler crash due to uninitialized binding definitions must
    // be avoided at all costs. The clarity of the complete mass of reported errors is of secondary importance.
    assertCompilationErrorSignatures(s"$fragmentBase/fallbacks.lore", exitCompilationEarly = false)(
      (classOf[SchemaFeedback.InheritanceCycle], 5),
      (classOf[SchemaFeedback.InheritanceCycle], 8),
      (classOf[AliasFeedback.StructExpected], 15),
      (classOf[AliasFeedback.ObjectExpected], 19),
      (classOf[TypingFeedback.Call.IllegalArity], 26),
    )
  }

}
