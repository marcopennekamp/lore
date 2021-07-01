package lore.compiler.phases.constraints

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.types.{Fit, TupleType, Type}

object MultiFunctionConstraints {

  /**
    * Verifies:
    *   1. For each function, first the input abstractness constraint and then the totality constraint.
    *   2. A child function's output type is a subtype of its parent function's output type.
    *
    * Note that uniqueness is already checked by the DeclarationResolver.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    (
      verifyAbstractness(mf),
      verifyOutputTypes(mf),
    ).simultaneous.verification
  }

  /**
    * Verifies for each function in the multi-function first the input abstractness constraint and then the totality
    * constraint.
    */
  private def verifyAbstractness(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    mf.functions.filter(_.isAbstract).map { function =>
      verifyInputAbstractness(function)
        .flatMap(_ => verifyTotalityConstraint(function, mf))
    }.simultaneous.verification
  }

  case class FunctionIllegallyAbstract(function: FunctionDefinition) extends Feedback.Error(function) {
    override def message: String = s"The function ${function.signature} is declared abstract even though it doesn't have an" +
      s" abstract input type. Either implement the function or ensure the input type is abstract."
  }

  /**
    * Verifies that the given function satisfies the input abstractness constraint.
    */
  private def verifyInputAbstractness(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    if (!Type.isAbstract(function.signature.inputType)) {
      Compilation.fail(FunctionIllegallyAbstract(function))
    } else Verification.succeed
  }

  case class AbstractFunctionNotImplemented(function: FunctionDefinition, missing: Vector[Type]) extends Feedback.Error(function) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies the totality constraint for the given function.
    */
  private def verifyTotalityConstraint(function: FunctionDefinition, mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
      val missing = verifyInputTypeTotality(mf, function.signature.inputType)
      if (missing.nonEmpty) Compilation.fail(AbstractFunctionNotImplemented(function, missing)) else Verification.succeed
  }

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
    *   that extend `Fish`.
    */
  private def verifyInputTypeTotality(mf: MultiFunctionDefinition, inputType: TupleType)(implicit registry: Registry): Vector[TupleType] = {
    Type.abstractResolvedDirectSubtypes(inputType).map(_.asInstanceOf[TupleType]).flatMap { subtype =>
      val isImplemented = mf.functions.exists { f2 =>
        Fit.isMoreSpecific(f2.signature.inputType, inputType) && mf.fit(subtype).contains(f2)
      }

      if (!isImplemented) {
        if (Type.isAbstract(subtype)) {
          verifyInputTypeTotality(mf, subtype)
        } else {
          Vector(subtype)
        }
      } else {
        Vector.empty
      }
    }
  }

  case class IncompatibleOutputTypes(
    child: FunctionSignature, parent: FunctionSignature, parentInstance: FunctionSignature,
  ) extends Feedback.Error(child) {
    override def message: String = s"The functions $parent and $child are in a hierarchical relationship, but the latter's" +
      s" output type is not a subtype of the former's output type. Concretely, it should hold that ${child.outputType} <:" +
      s" ${parentInstance.outputType}, but this is not the case."
  }

  /**
    * Verifies that the output types of the functions in the multi-function are compatible with each other. That is, a
    * child's output type must be a subtype of the parent's output type.
    *
    * This gets slightly more complicated with polymorphic output types. If a parent function has a polymorphic output
    * type, we have to allocate the variables according to the "argument" types provided by the child function. An
    * example will clear this up:
    *
    *     function identity(x: A): A = x
    *     function identity(x: Int): Int = x
    *
    * This should compile, because since we assign A = Int for the second function, its return type also has to be a
    * subtype of A = Int. That is the case here. Intuitively, it makes sense to specialize a function in this way: The
    * more general function states a contract between the input and output types: "Given an input of type A, the output
    * must also be of type A." The specializing function fulfills this contract, since it takes and returns a value of
    * the same type.
    *
    * TODO: This is yet a bit more complicated when the return type of the child function is inferred via the
    *       type variable allocation. See [[lore.compiler.types.TypeVariableAllocation.of]] with the genericListify
    *       example.
    */
  private def verifyOutputTypes(mf: MultiFunctionDefinition): Verification = {
    def verifyHierarchyNode(node: mf.hierarchy.graph.NodeT): Verification = {
      val parent = node.value
      val successors = node.diSuccessors.toVector
      successors.map { successor =>
        val child = successor.value
        parent.instantiate(child.signature.inputType).flatMap { parentInstance =>
          val errors = if (child.signature.outputType </= parentInstance.signature.outputType) {
            Vector(IncompatibleOutputTypes(child.signature, parent.signature, parentInstance.signature))
          } else Vector.empty
          Verification.fromErrors(errors).flatMap(_ => successors.map(verifyHierarchyNode).simultaneous.verification)
        }
      }.simultaneous.verification
    }

    mf.hierarchy.roots.map(verifyHierarchyNode).simultaneous.verification
  }

}
