package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Error
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.Type.isAbstract
import lore.compiler.types.{Ards, Fit, Type}

object MultiFunctionConstraints {

  /**
    * Verifies:
    *   1. The input abstractness constraint.
    *   2. The totality constraint.
    *   3. A child function's output type is a subtype of its parent function's output type.
    *
    * Note that uniqueness is already checked by the DeclarationResolver.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    (
      verifyInputAbstractness(mf),
      verifyTotalityConstraint(mf),
      verifyOutputTypes(mf),
    ).simultaneous.verification
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
      mf.functions.filter(_.isAbstract).filterNot(f => Type.isAbstract(f.signature.inputType)).map(FunctionIllegallyAbstract)
    }
  }

  case class AbstractFunctionNotTotal(function: FunctionDefinition, missing: Vector[Type]) extends Error(function) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies the multi-function for the totality constraint.
    */
  def verifyTotalityConstraint(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    // TODO: Refactor this with the new fit changes. (It will be a fucking shit-show, no doubt.)

    /**
      * Verifies whether the given input type of an abstract function is covered by specialized functions. If this
      * verification fails, a list of input types for which the function has to be implemented is returned.
      *
      * Some notes on the implementation:
      *
      * - The algorithm looks at all the abstract-resolved direct subtypes of the input type. An abstract parameter
      *   can be checked by finding an implementation that covers each of its subtypes. A concrete parameter, on the
      *   other hand, cannot be covered solely by implementing functions for subtypes, as the concrete type itself
      *   may be instanced without being one of its subtypes. Hence, the algorithm is restricted to checking ARDS.
      * - For each subtype, the algorithm first tries to find another function that covers the subtype. This may be
      *   another abstract function or a concrete function.
      * - If such a function cannot be found and the subtype is abstract, we assume that the subtype is supposed to
      *   be an input type of an implicit abstract function. For example, we might declare an abstract function `name`
      *   for a trait `Animal`, and another trait `Fish` without wishing to have to redeclare the function `name`. This
      *   special case in the algorithm makes it possible to check the totality of the `name(animal: Animal)` function
      *   without having to declare a function `name(fish: Fish)`. We merely have to declare the function for all types
      *   that extend/implement `Fish`.
      */
    def verifyInputType(inputType: Type): Vector[Type] = {
      Ards.abstractResolvedDirectSubtypes(inputType).toVector.flatMap { subtype =>
        // TODO: Can we optimize this given the new hierarchy?
        val isImplemented = mf.functions.exists { f2 =>
          Fit.isMoreSpecific(f2.signature.inputType, inputType) && mf.fit(subtype).contains(f2)
        }

        if (!isImplemented) {
          if (Type.isAbstract(subtype)) {
            verifyInputType(subtype)
          } else {
            Vector(subtype)
          }
        } else {
          Vector.empty
        }
      }
    }

    Verification.fromErrors {
      mf.functions.filter(_.isAbstract).flatMap { function =>
        val missing = verifyInputType(function.signature.inputType)
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
      val successors = node.diSuccessors.toVector
      successors.map { successor =>
        val child = successor.value
        val errors = if (!(child.signature.outputType <= parent.signature.outputType)) {
          Vector(IncompatibleOutputTypes(child, parent))
        } else Vector.empty
        Verification.fromErrors(errors).flatMap(_ => successors.map(verifyHierarchyNode).simultaneous.verification)
      }.simultaneous.verification
    }

    mf.hierarchyRoots.map(verifyHierarchyNode).simultaneous.verification
  }

}
