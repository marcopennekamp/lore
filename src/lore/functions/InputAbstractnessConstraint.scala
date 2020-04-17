package lore.functions

import lore.compiler.Compilation.Verification
import lore.compiler.{Error, Registry}
import lore.definitions.{FunctionDefinition, MultiFunctionDefinition}

object InputAbstractnessConstraint {
  // TODO: Actually apply this verification.

  /**
    * Checks whether the given multi-function satisfies the input abstractness constraint.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    Verification(mf.functions.filter(_.isAbstract).filterNot(_.inputType.isAbstract).map(FunctionIllegallyAbstract))
  }

  case class FunctionIllegallyAbstract(function: FunctionDefinition) extends Error(function.position) {
    override def message: String = s"The function ${function.signature} is declared abstract even though it doesn't have an" +
      s" abstract input type. Either implement the function or ensure the input type is abstract."
  }
}
