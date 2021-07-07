package lore.compiler.phases.constraints

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.functions.FunctionSignature

object SignatureConstraints {

  /**
    * Verifies for the given signature:
    *   1. All parameters have unique names.
    */
  def verify(signature: FunctionSignature): Verification = {
    verifyUnique(signature)
  }

  case class NonUniqueParameterName(signature: FunctionSignature, name: String) extends Feedback.Error(signature.position) {
    override def message: String = s"This function ${signature.name} has two or more parameters named $name. Parameter names must be unique."
  }

  private def verifyUnique(signature: FunctionSignature): Verification = {
    signature.parameters.map(_.name).groupBy(identity).map {
      case (_, Vector(_)) => Verification.succeed
      case (name, _) => Compilation.fail(NonUniqueParameterName(signature, name))
    }.toVector.simultaneous.verification
  }

}
