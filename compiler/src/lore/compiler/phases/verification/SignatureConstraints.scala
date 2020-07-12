package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Error
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
      case (name, _) => Some(NonUniqueParameterName(signature, name))
    }.toList
    Verification.fromErrors(errors)
  }

  case class NonUniqueParameterName(signature: FunctionSignature, name: String) extends Error(signature.position) {
    override def message: String = s"This function ${signature.name} has two or more parameters named $name. Parameter names must be unique."
  }
}
