package lore.compiler.constraints.aliases

import lore.compiler.feedback.AliasFeedback
import lore.compiler.test.BaseSpec

class AliasConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/aliases"

  s"$fragmentBase/aliases" should "be compiled with alias constraint errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/aliases.lore")(
      (classOf[AliasFeedback.StructExpected], 6),
      (classOf[AliasFeedback.ObjectExpected], 10),
      (classOf[AliasFeedback.ConstantObjectAliasExpected], 10),
    )
  }
}
