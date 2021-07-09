package lore.compiler.phases.constraints

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.types._
import scalaz.std.vector._
import scalaz.syntax.traverse._

object MultiFunctionConstraints {

  /**
    * Verifies:
    *   1. Parameter uniqueness for each function.
    *   2. Abstractness constraints for each function.
    *   3. Return constraints for each function.
    *   4. A child function's output type is a subtype of its parent function's output type.
    *
    * Note that uniqueness of input types is already checked by the DeclarationResolver.
    */
  def verify(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyParameterUniqueness(mf)
    verifyAbstractness(mf)
    verifyOutputTypes(mf)
    verifyReturnConstraints(mf)
  }

  /**
    * Verifies for each function that parameter names are unique.
    */
  private def verifyParameterUniqueness(mf: MultiFunctionDefinition)(implicit reporter: Reporter): Unit = {
    mf.functions.map(_.signature).foreach(verifyUniqueParameterNames)
  }

  case class DuplicateParameterName(signature: FunctionSignature, name: String) extends Feedback.Error(signature.position) {
    override def message: String = s"The function ${signature.name} has two or more parameters named $name. Parameter names must be unique."
  }

  private def verifyUniqueParameterNames(signature: FunctionSignature)(implicit reporter: Reporter): Unit = {
    signature.parameters.verifyUnique(_.name, parameter => DuplicateParameterName(signature, parameter.name))
  }

  /**
    * Verifies for each function in the multi-function first the input abstractness constraint and then the totality
    * constraint.
    */
  private def verifyAbstractness(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    mf.functions.filter(_.isAbstract).foreach { function =>
      MemoReporter.chain(reporter)(
        implicit reporter => verifyInputAbstractness(function),
        implicit reporter => verifyTotalityConstraint(function, mf),
      )
    }
  }

  case class FunctionIllegallyAbstract(function: FunctionDefinition) extends Feedback.Error(function) {
    override def message: String = s"The function ${function.signature} is declared abstract even though it doesn't have an" +
      s" abstract input type. Either implement the function or ensure the input type is abstract."
  }

  /**
    * Verifies that the given function satisfies the input abstractness constraint.
    */
  private def verifyInputAbstractness(function: FunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    if (Type.isConcrete(function.signature.inputType)) {
      reporter.error(FunctionIllegallyAbstract(function))
    }
  }

  case class AbstractFunctionNotImplemented(function: FunctionDefinition, missing: Vector[Type]) extends Feedback.Error(function) {
    override def message: String = s"The abstract function ${function.signature} is not fully implemented and thus doesn't" +
      s" satisfy the totality constraint. Please implement functions for the following input types: ${missing.mkString(", ")}."
  }

  /**
    * Verifies the totality constraint for the given function.
    */
  private def verifyTotalityConstraint(function: FunctionDefinition, mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    val missing = verifyInputTypeTotality(function.signature.inputType, mf)
    if (missing.nonEmpty) {
      reporter.error(AbstractFunctionNotImplemented(function, missing))
    }
  }

  /**
    * Verifies whether the given input type is covered by specialized functions. If this verification fails, a list of
    * input types for which the function has to be implemented is returned.
    *
    * The implementation first collects all relevant subtypes of the given input type. For each such subtype, there
    * must be a function `f2` in `mf.fit(subtype)` that specializes `function`.
    *
    * If `f2` cannot be found, but the subtype is abstract, taking the direct subtype didn't quite pan out. In such a
    * case, we take the relevant subtypes of the abstract subtype, and then verify their totality. For example, we
    * might declare an abstract function `name(animal: Animal)` for a trait `Animal`, and another trait `Fish` without
    * wishing to have to redeclare the function `name`. This special case in the algorithm makes it possible to check
    * the totality of the `name(animal: Animal)` function without having to declare a function `name(fish: Fish)`. We
    * merely have to define the function for all types that extend `Fish`.
    */
  private def verifyInputTypeTotality(inputType: Type, mf: MultiFunctionDefinition)(implicit registry: Registry): Vector[TupleType] = {
    val subtypes = relevantSubtypes(inputType).map(_.asInstanceOf[TupleType])
    //Feedback.logger.debug(s"Totality constraint: Checking ${subtypes.size} relevant subtypes for input type $inputType.")
    subtypes.flatMap { subtype =>
      if (mf.fit(subtype).exists(f2 => Fit.isMoreSpecific(f2.signature.inputType, inputType))) {
        Vector.empty
      } else if (Type.isAbstract(subtype)) {
        verifyInputTypeTotality(subtype, mf)
      } else Vector(subtype)
    }
  }

  /**
    * Returns a set of subtypes of the given type that should be checked for the totality constraint. These are usually
    * the direct subtypes, but may also be all concrete subtypes in the case of intersection types. If `tpe` isn't
    * abstract, the function simply returns `tpe`.
    *
    * Looking at the direct subtypes is usually preferable as long as correctness is preserved. Having to enumerate all
    * concrete subtypes can quickly go out of hand. For example, suppose we have a tuple (A, B, C) and A, B, and C each
    * have about 250 concrete subtypes. (This scenario is not unlikely for core library traits that are extended by
    * user code.) Then we'd have to check 250*250*250 = 15625000 combinations. Suddenly, the compiler will choke when
    * the user defines an abstract function `foobar(iterable: Iterable, key: Hashable, savable: Savable)`.
    *
    * When dealing with intersection types, we cannot simpy consider direct subtypes because structs can have complex,
    * multi-layered trait inheritance relationships. Take the following example into account:
    *
    * {{{
    * trait U
    * trait U1 extends U
    * trait U2 extends U
    * trait V
    * trait V1 extends V
    *
    * struct A extends V
    * struct B extends U1, V1
    *
    * function f(value: U & V): String
    * }}}
    *
    * First of all, one might intuitively think that we only have to check these direct subtypes of `U & V`: `U1 & V1`,
    * `U1 & A`, `U2 & V1`, `U2 & A`. But that is wrong! A value of type `U1 & A` cannot ever be passed to `f`, because
    * `A` does not extend `U1`. Clearly, creating a faux subtype `U1 & A` from `U & V` is the wrong approach.
    *
    * Instead, we have to look at all concrete subtypes of `U` and `V` separately, and then filter out any that aren't
    * subtypes of `U & V`. This gives us all concrete subtypes that can inhabit `U & V`. In the example above, we look
    * at `A` and `B` via `V` and `B` via `U`. `A` is filtered out, because `A </= U & V`, and only `B` is deemed a
    * relevant subtype.
    */
  private def relevantSubtypes(tpe: Type)(implicit registry: Registry): Vector[Type] = tpe match {
    case t if Type.isConcrete(t) => Vector(t)
    case dt: DeclaredType => registry.declaredTypeHierarchy.getDirectSubtypes(dt)
    case SumType(parts) => parts.flatMap(relevantSubtypes).toVector
    case IntersectionType(_) => concreteSubtypes(tpe)
    case TupleType(elements) => elements.map(relevantSubtypes).sequence.map(TupleType(_))
  }

  /**
    * Returns a set of all concrete subtypes of the given type. If `tpe` isn't abstract, the function simply returns
    * `tpe`.
    */
  private def concreteSubtypes(tpe: Type)(implicit registry: Registry): Vector[Type] = tpe match {
    case t if Type.isConcrete(t) => Vector(t)
    case dt: DeclaredType => registry.declaredTypeHierarchy.getConcreteSubtypes(dt)
    case SumType(parts) => parts.flatMap(concreteSubtypes).toVector
    case IntersectionType(parts) => parts.flatMap(concreteSubtypes).toVector.filter(subtype => subtype <= tpe)
    case TupleType(elements) => elements.map(concreteSubtypes).sequence.map(TupleType(_))
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
  private def verifyOutputTypes(mf: MultiFunctionDefinition)(implicit reporter: Reporter): Unit = {
    def verifyHierarchyNode(node: mf.hierarchy.graph.NodeT): Unit = {
      val parent = node.value
      val successors = node.diSuccessors.toVector
      successors.foreach { successor =>
        val child = successor.value
        parent.instantiate(child.signature.inputType).foreach { parentInstance =>
          if (child.signature.outputType <= parentInstance.signature.outputType) {
            successors.foreach(verifyHierarchyNode)
          } else {
            reporter.error(IncompatibleOutputTypes(child.signature, parent.signature, parentInstance.signature))
          }
        }
      }
    }

    mf.hierarchy.roots.foreach(verifyHierarchyNode)
  }

  /**
    * Verifies return constraints for all functions defined in the given multi-function.
    */
  private def verifyReturnConstraints(mf: MultiFunctionDefinition)(implicit reporter: Reporter): Unit = {
    mf.functions.flatMap(_.bodyNode).foreach(ReturnConstraints.verify)
  }

}
