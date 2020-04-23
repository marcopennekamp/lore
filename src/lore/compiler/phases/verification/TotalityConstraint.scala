package lore.compiler.phases.verification

import lore.compiler.Compilation.Verification
import lore.compiler.Registry
import lore.compiler.feedback.Error
import lore.definitions.{FunctionDefinition, MultiFunctionDefinition}
import lore.types.{Subtyping, Type}

object TotalityConstraint {
  /*
   * We have the following interesting case:
   *   Say we have types abstract X, A < X, B < X, C < X and a component +T. We have an abstract function with
   *   input X & +T that is implemented by a function with input (A | B) & +T, and a function with input C & +T.
   *   The totality constraint is checked such that the above situation is legal. This is implemented in the file
   *   abstract-sum-totality.lore.
   */

  /**
    * Verifies the multi-function for the totality constraint.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    Verification(mf.functions.filter(_.isAbstract).flatMap { function =>
      val missing = isVerified(mf)(function)
      if (missing.nonEmpty) Some(AbstractFunctionNotTotal(function, missing)) else None
    })
  }

  case class AbstractFunctionNotTotal(function: FunctionDefinition, missing: List[Type]) extends Error(function.position) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies whether the given abstract function satisfies the totality constraint. If the totality constraint is
    * satisfied, an empty list is returned. Otherwise, a list of input types for which a function has to be implemented
    * is returned.
    */
  private def isVerified(mf: MultiFunctionDefinition)(f: FunctionDefinition)(implicit registry: Registry): List[Type] = {
    // We need to use the direct declared subtypes of the ABSTRACT parameters. An example will clear this up:
    //    Say we have types abstract A, A1 <: A, A2 <: A, non-abstract B, B1 <: B, B2 <: B.
    //    An abstract function f(a: A, b: B) with a non-abstract B must also cover the case f(a: AX, b: B), not just for
    //    B1 and B2, if the given value of type B is neither B1 nor B2, since it could just be B. Hence, we cannot use
    //    directDeclaredSubtypes, because it would substitute B1 and B2 for B, leaving B out of the equation entirely.
    Subtyping.abstractResolvedDirectSubtypes(f.inputType).toList.flatMap { subtype =>
      val isValid = mf.functions.exists { f2 =>
        Subtyping.isStrictSubtype(f2.inputType, f.inputType) && mf.fit(subtype).contains(f2)
      }
      if (!isValid) Some(subtype) else None
    }
  }
}
