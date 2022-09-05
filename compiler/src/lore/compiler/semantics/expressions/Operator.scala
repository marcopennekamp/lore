package lore.compiler.semantics.expressions

sealed trait Operator

sealed trait UnaryOperator extends Operator

object UnaryOperator {
  case object Negation extends UnaryOperator
  case object LogicalNot extends UnaryOperator
}

sealed trait BinaryOperator extends Operator

object BinaryOperator {
  case object Addition extends BinaryOperator
  case object Subtraction extends BinaryOperator
  case object Multiplication extends BinaryOperator
  case object Division extends BinaryOperator
  case object Equals extends BinaryOperator
  case object LessThan extends BinaryOperator
  case object LessThanEquals extends BinaryOperator
  case object Append extends BinaryOperator
}

sealed trait XaryOperator extends Operator

object XaryOperator {
  case object Conjunction extends XaryOperator
  case object Disjunction extends XaryOperator
  case object Concatenation extends XaryOperator
}
