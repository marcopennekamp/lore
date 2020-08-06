package lore.compiler.semantics.functions

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionDefinition.CannotInstantiateFunction
import lore.compiler.semantics.{TypeScope, functions}
import lore.compiler.syntax.ExprNode
import lore.compiler.types.{Fit, Type}

/**
  * A definition of a single function as part of a larger multi-function.
  *
  * Function definition equality is always reference equality, as we create exactly one function definition
  * for every defined function.
  *
  * @param typeScope The scope that saves type variables declared with the function.
  */
class FunctionDefinition(
  val name: String, val typeScope: TypeScope, val parameters: List[ParameterDefinition], outputType: Type,
  val bodyNode: Option[ExprNode], override val position: Position,
) extends Positioned {
  val isAbstract: Boolean = bodyNode.isEmpty
  lazy val signature: FunctionSignature = functions.FunctionSignature(name, parameters, outputType, position)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var body: Option[Expression] = _

  /**
    * Attempts to instantiate the function definition with the given argument type.
    */
  def instantiate(argumentType: Type): Compilation[FunctionInstance] = {
    Fit.assignments(argumentType, signature.inputType) match {
      case None => Compilation.fail(CannotInstantiateFunction(this, argumentType))
      case Some(assignments) => FunctionInstance(this, signature.substitute(assignments)).compiled
    }
  }
}

object FunctionDefinition {
  case class CannotInstantiateFunction(definition: FunctionDefinition, argumentType: Type) extends Error(definition) {
    override def message = s"The function definition $definition cannot be instantiated from argument type $argumentType."
  }
}
