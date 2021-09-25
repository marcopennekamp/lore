package lore.compiler.constraints

import lore.compiler.feedback.{AliasFeedback, Reporter}
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.types.StructType

object AliasConstraints {

  /**
    * Verifies that, if the type alias is a struct alias, the underlying type is a struct type.
    */
  def verify(alias: AliasDefinition)(implicit reporter: Reporter): Unit = {
    if (alias.isStructAlias && !alias.schema.originalType.isInstanceOf[StructType]) {
      reporter.error(AliasFeedback.StructExpected(alias))
    }
  }

}
