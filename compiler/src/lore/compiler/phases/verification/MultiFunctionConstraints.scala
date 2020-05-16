package lore.compiler.phases.verification

import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.types.CompilerSubtyping
import lore.compiler.{Compilation, Fragment, Registry}
import lore.compiler.definitions.{FunctionDefinition, MultiFunctionDefinition}
import lore.types.Type

object MultiFunctionConstraints {
  case class FunctionAlreadyExists(definition: FunctionDefinition) extends Error(definition) {
    override def message = s"The function ${definition.signature} is already declared somewhere else."
  }

  case class FunctionIllegallyAbstract(function: FunctionDefinition) extends Error(function) {
    override def message: String = s"The function ${function.signature} is declared abstract even though it doesn't have an" +
      s" abstract input type. Either implement the function or ensure the input type is abstract."
  }

  case class AbstractFunctionNotTotal(function: FunctionDefinition, missing: List[Type]) extends Error(function) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies:
    *   1. All functions have a unique signature.
    *   2. No function body contains a continuation node.
    *   3. The input abstractness constraint.
    *   4. The totality constraint.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    (
      verifyUnique(mf),
      verifyNoContinuation(mf),
      verifyInputAbstractness(mf),
      verifyTotalityConstraint(mf),
      // TODO: Verify that return types are subtypes of return types of functions higher in the hierarchy.
    ).simultaneous.verification
  }

  /**
    * Verifies that all functions declared in the multi-function have a unique signature.
    */
  def verifyUnique(mf: MultiFunctionDefinition): Verification = {
    // Of course, all functions added to the multi-function must have the same name. If that is not the case,
    // there is something very wrong with the compiler.
    mf.functions.foreach(function => assert(function.name == mf.name))

    // Then verify that all functions have different signatures.
    mf.functions.map { function =>
      if (mf.functions.filterNot(_ == function).map(_.signature).contains(function.signature)) {
        // We have found a function with a duplicated signature!
        Compilation.fail(FunctionAlreadyExists(function))
      } else {
        Verification.succeed
      }
    }.simultaneous.verification
  }

  /**
    * Verifies that none of the functions contain a continuation node in their bodies.
    */
  def verifyNoContinuation(mf: MultiFunctionDefinition): Verification = {
    mf.functions.map { function =>
      implicit val fragment: Fragment = function.position.fragment
      function.body match {
        case None => Verification.succeed
        case Some(expression) => StmtVisitor.visit(new NoContinuationVisitor())(expression)
      }
    }.simultaneous.verification
  }

  /**
    * Checks whether the given multi-function satisfies the input abstractness constraint.
    */
  def verifyInputAbstractness(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    Verification.fromErrors {
      mf.functions.filter(_.isAbstract).filterNot(_.signature.inputType.isAbstract).map(FunctionIllegallyAbstract)
    }
  }

  /**
    * Verifies the multi-function for the totality constraint.
    */
  def verifyTotalityConstraint(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    //  We have the following interesting case:
    //  Say we have types abstract X, A < X, B < X, C < X and a component +T. We have an abstract function with
    //  input X & +T that is implemented by a function with input (A | B) & +T, and a function with input C & +T.
    //  The totality constraint is checked such that the above situation is legal. This is implemented in the file
    //  abstract-sum-totality.lore.

    /**
      * Verifies whether the given abstract function satisfies the totality constraint. If the totality constraint is
      * satisfied, an empty list is returned. Otherwise, a list of input types for which a function has to be implemented
      * is returned.
      */
    def verifyFunction(f: FunctionDefinition): List[Type] = {
      // We need to use the direct declared subtypes of the ABSTRACT parameters. An example will clear this up:
      //    Say we have types abstract A, A1 <: A, A2 <: A, non-abstract B, B1 <: B, B2 <: B.
      //    An abstract function f(a: A, b: B) with a non-abstract B must also cover the case f(a: AX, b: B), not just for
      //    B1 and B2, if the given value of type B is neither B1 nor B2, since it could just be B. Hence, we cannot use
      //    directDeclaredSubtypes, because it would substitute B1 and B2 for B, leaving B out of the equation entirely.
      CompilerSubtyping.abstractResolvedDirectSubtypes(f.signature.inputType).toList.flatMap { subtype =>
        // TODO: Can we optimize this given the new hierarchy?
        val isValid = mf.functions.exists { f2 =>
          CompilerSubtyping.isStrictSubtype(f2.signature.inputType, f.signature.inputType) && mf.fit(subtype).contains(f2)
        }
        if (!isValid) Some(subtype) else None
      }
    }

    Verification.fromErrors {
      mf.functions.filter(_.isAbstract).flatMap { function =>
        val missing = verifyFunction(function)
        if (missing.nonEmpty) Some(AbstractFunctionNotTotal(function, missing)) else None
      }
    }
  }
}
