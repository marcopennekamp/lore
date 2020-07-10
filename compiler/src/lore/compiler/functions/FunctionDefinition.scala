package lore.compiler.functions

import lore.compiler.ast.ExprNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, TypeScope}
import lore.compiler.feedback.{Error, Position, Positioned}
import lore.compiler.functions
import lore.compiler.functions.FunctionDefinition.CannotInstantiateFunction
import lore.compiler.types.{Fit, Type}

/**
  * @param typeScope The scope that saves type variables declared with the function.
  */
class FunctionDefinition(
  val name: String, val typeScope: TypeScope, val parameters: List[ParameterDefinition], outputType: Type,
  val body: Option[ExprNode], override val position: Position,
) extends Positioned {
  val isAbstract: Boolean = body.isEmpty
  lazy val signature: FunctionSignature = functions.FunctionSignature(name, parameters, outputType)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"

  /**
    * Attempts to instantiate the function definition with the given argument type.
    */
  def instantiate(argumentType: Type): C[FunctionInstance] = {
    Fit.assignments(argumentType, signature.inputType) match {
      case None => Compilation.fail(CannotInstantiateFunction(this, argumentType))
      case Some(assignments) => Compilation.succeed(FunctionInstance(this, signature.substitute(assignments)))
    }
  }

  /**
    * Function definition equality is always reference equality, as we create exactly one function definition
    * for every defined function.
    */
  override def equals(obj: Any): Boolean = obj match {
    case other: FunctionDefinition => this.eq(other)
    case _ => false
  }
  override lazy val hashCode: Int = signature.hashCode
}

object FunctionDefinition {
  case class CannotInstantiateFunction(definition: FunctionDefinition, argumentType: Type) extends Error(definition) {
    override def message = s"The function definition $definition cannot be instantiated from argument type $argumentType."
  }
}
