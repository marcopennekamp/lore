package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Error
import lore.compiler.semantics.functions.FunctionSignature

object SignatureConstraints {

  /**
   * Verifies for the given signature:
   *   1. All parameters have unique names.
   */
  def verify(signature: FunctionSignature): Verification = {
    verifyUnique(signature)
  }

  case class NonUniqueParameterName(signature: FunctionSignature, name: String) extends Error(signature.position) {
    override def message: String = s"This function ${signature.name} has two or more parameters named $name. Parameter names must be unique."
  }

  private def verifyUnique(signature: FunctionSignature): Verification = {
    val errors = signature.parameters.map(_.name).groupBy(identity).flatMap {
      case (_, Vector(_)) => None
      case (name, _) => Some(NonUniqueParameterName(signature, name))
    }.toVector
    Verification.fromErrors(errors)
  }

}
