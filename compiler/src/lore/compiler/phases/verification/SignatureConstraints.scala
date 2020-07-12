package lore.compiler.phases.verification

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.{Error, Position}
import lore.compiler.functions.FunctionSignature

object SignatureConstraints {
  /**
   * Verifies for the given signature:
   *   1. All parameters have unique names.
   */
  def verify(signature: FunctionSignature): Verification = {
    verifyUnique(signature)
  }

  private def verifyUnique(signature: FunctionSignature): Verification = {
    val errors = signature.parameters.map(_.name).groupBy(identity).flatMap {
      case (_, List(_)) => None
      case (name, _) => Some(NonUniqueParameterName(name, signature.position))
    }.toList
    if (errors.isEmpty) Verification.succeed else Compilation.fail(errors: _*)
  }

  case class NonUniqueParameterName(name: String, pos: Position) extends Error(pos) {
    override def message: String = s"This function has two or more parameters named $name. Parameter names must be unique."
  }
}
