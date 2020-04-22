package lore.compiler.phases

import lore.compiler.Compilation._
import lore.compiler.Registry

class Phase3()(implicit registry: Registry) extends Phase[Unit] {
  override def result: Verification = {
    val withVerifiedConstraints = (
      // Verify declared type constraints.
      registry.getTypeDefinitions.values.map(_.verifyConstraints).toList.simultaneous,
      // Verify multi-function constraints.
      registry.getMultiFunctions.values.map(_.verifyConstraints).toList.simultaneous,
    ).simultaneous

    // TODO: Type all function/constructor bodies.
    val withTypedFunctions = withVerifiedConstraints

    withTypedFunctions.map(_ => ())
  }
}
