package lore.compiler.semantics.functions

import lore.compiler.poem.PoemIntrinsic
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.StructConstructorBinding

/**
  * A call target models the callable entity in a [[Expression.Call]].
  */
sealed trait CallTarget {
  def getExpression: Option[Expression] = None
  def withExpression(expression: Option[Expression]): CallTarget = this
}

object CallTarget {

  /**
    * Represents multi-functions as call targets.
    */
  case class MultiFunction(mf: MultiFunctionDefinition) extends CallTarget {
    override def toString: String = mf.name.toString
  }

  /**
    * Represents calling any kind of function value.
    */
  case class Value(expression: Expression) extends CallTarget {
    override def getExpression: Option[Expression] = Some(expression)
    override def withExpression(expression: Option[Expression]): CallTarget = this.copy(expression = expression.get)
    override def toString: String = expression.toString
  }

  /**
    * Represents struct constructors as call targets. Constructor values that are immediately called are instead
    * handled by [[CallTarget.Value]].
    */
  case class Constructor(binding: StructConstructorBinding) extends CallTarget {
    override def toString: String = binding.name.toString
  }

  /**
    * TODO (assembly): Rename this to Intrinsic.
    *
    * A dynamic call target, meaning that we trust in the runtime to provide the correct bindings. We don't know
    * anything about the input type.
    */
  case class Dynamic(intrinsic: PoemIntrinsic) extends CallTarget {
    override def toString: String = intrinsic.name
  }

}
