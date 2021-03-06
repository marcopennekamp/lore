package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionDefinition.CannotInstantiateFunction
import lore.compiler.semantics.scopes.LocalTypeScope
import lore.compiler.syntax.ExprNode
import lore.compiler.target.{Target, TargetIdentifiable}
import lore.compiler.types.{Fit, Type}

/**
  * A definition of a single function as part of a larger multi-function.
  *
  * Function definition equality is always reference equality, as we create exactly one function definition
  * for every defined function.
  *
  * The position is restricted to the function's name for better error highlighting and index building.
  *
  * @param typeScope The scope that saves type variables declared with the function.
  */
class FunctionDefinition(
  val signature: FunctionSignature,
  val typeScope: LocalTypeScope,
  val bodyNode: Option[ExprNode],
) extends Positioned with TargetIdentifiable {
  override val position: Position = signature.position
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${signature.parameters.mkString(", ")})"

  val name: String = signature.name
  val isAbstract: Boolean = bodyNode.isEmpty
  val isPolymorphic: Boolean = signature.isPolymorphic
  val isMonomorphic: Boolean = signature.isMonomorphic

  override val targetVariable: Target.Variable = RuntimeNames.functionDefinition(this)

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var body: Option[Expression] = _

  /**
    * Attempts to instantiate the function definition with the given argument type. If this is not possible, reports a
    * "cannot instantiate function" error.
    */
  def instantiate(argumentType: Type)(implicit reporter: Reporter): Option[FunctionInstance] = {
    val option = Fit
      .assignments(argumentType, signature.inputType)
      .map(assignments => FunctionInstance(this, signature.substitute(assignments)))

    if (option.isEmpty) {
      reporter.error(CannotInstantiateFunction(this, argumentType))
    }
    option
  }
}

object FunctionDefinition {
  case class CannotInstantiateFunction(definition: FunctionDefinition, argumentType: Type) extends Feedback.Error(definition) {
    override def message = s"The function definition $definition cannot be instantiated from argument type $argumentType."
  }
}
