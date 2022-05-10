package lore.compiler.constraints.aliases

import lore.compiler.feedback.AliasFeedback
import lore.compiler.test.BaseSpec

class AliasConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/aliases"

  "constraints/aliases/struct-aliases" should "be compiled with a struct alias error" in {
    assertCompilationErrorSignatures(s"$fragmentBase/struct_aliases.lore")(
      (classOf[AliasFeedback.StructExpected], 6),
    )
  }
}
