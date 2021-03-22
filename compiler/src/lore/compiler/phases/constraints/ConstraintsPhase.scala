package lore.compiler.phases.constraints

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry

object ConstraintsPhase {

  def process(implicit registry: Registry): Verification = {
    val typeDefinitions = registry.getTypeDefinitions.values.toVector
    val multiFunctions = registry.getMultiFunctions.values.toVector
    (
      typeDefinitions.map(DeclaredTypeConstraints.verify).simultaneous,
      multiFunctions.map(MultiFunctionConstraints.verify).simultaneous,
      multiFunctions.flatMap(_.functions.flatMap(_.bodyNode)).map(ReturnConstraints.verify).simultaneous,
    ).simultaneous.verification
  }

}
