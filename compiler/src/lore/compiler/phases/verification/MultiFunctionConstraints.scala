package lore.compiler.phases.verification

import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Fragment, Registry}
import lore.compiler.core.feedback.Error
import lore.compiler.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.{Ards, Fit, Type}

object MultiFunctionConstraints {
  /**
    * Verifies:
    *   1. No function body contains a continuation node.
    *   2. The input abstractness constraint.
    *   3. The totality constraint.
    *   4. A child function's output type is a subtype of its parent function's output type.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    (
      // Note that uniqueness is already checked in the DeclarationResolver.
      verifyNoContinuation(mf),
      verifyInputAbstractness(mf),
      verifyTotalityConstraint(mf),
      verifyOutputTypes(mf),
    ).simultaneous.verification
  }

  /**
    * Verifies that none of the functions contain a continuation node in their bodies.
    */
  def verifyNoContinuation(mf: MultiFunctionDefinition): Verification = {
    mf.functions.map { function =>
      function.body match {
        case None => Verification.succeed
        case Some(expression) => StmtVisitor.visit(new NoContinuationVisitor)(expression)
      }
    }.simultaneous.verification
  }

  case class FunctionIllegallyAbstract(function: FunctionDefinition) extends Error(function) {
    override def message: String = s"The function ${function.signature} is declared abstract even though it doesn't have an" +
      s" abstract input type. Either implement the function or ensure the input type is abstract."
  }

  /**
    * Checks whether the given multi-function satisfies the input abstractness constraint.
    */
  def verifyInputAbstractness(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    Verification.fromErrors {
      mf.functions.filter(_.isAbstract).filterNot(_.signature.inputType.isAbstract).map(FunctionIllegallyAbstract)
    }
  }

  case class AbstractFunctionNotTotal(function: FunctionDefinition, missing: List[Type]) extends Error(function) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies the multi-function for the totality constraint.
    */
  def verifyTotalityConstraint(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    // TODO: Refactor this with the new fit changes. (It will be a fucking shit-show, no doubt.)

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
      Ards.abstractResolvedDirectSubtypes(f.signature.inputType).toList.flatMap { subtype =>
        // TODO: Can we optimize this given the new hierarchy?
        val isValid = mf.functions.exists { f2 =>
          Fit.isMoreSpecific(f2.signature.inputType, f.signature.inputType) && mf.fit(subtype).contains(f2)
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

  case class IncompatibleOutputTypes(child: FunctionDefinition, parent: FunctionDefinition) extends Error(child) {
    override def message: String = s"The functions ${parent.signature} and ${child.signature} are in a hierarchical relationship, but the latter's" +
      s" output type is not a subtype of the former's output type. Concretely, it should hold that ${child.signature.outputType} <:" +
      s" ${parent.signature.outputType}, but this is not the case."
  }

  /**
    * Verifies that the output types of the functions in the multi-function's hierarchy are compatible with each
    * other. That is, a hierarchy child's output type must be a subtype of its hierarchy parent's output type.
    */
  def verifyOutputTypes(mf: MultiFunctionDefinition): Verification = {
    def verifyHierarchyNode(node: mf.hierarchy.NodeT): Verification = {
      val parent = node.value
      val successors = node.diSuccessors.toList
      successors.map { successor =>
        val child = successor.value
        val errors = if (!(child.signature.outputType <= parent.signature.outputType)) {
          List(IncompatibleOutputTypes(child, parent))
        } else Nil
        Verification.fromErrors(errors).flatMap(_ => successors.map(verifyHierarchyNode).simultaneous.verification)
      }.simultaneous.verification
    }

    mf.hierarchyRoots.map(verifyHierarchyNode).simultaneous.verification
  }
}
