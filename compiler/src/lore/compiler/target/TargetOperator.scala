package lore.compiler.target

sealed trait TargetOperator

object TargetOperator {
  case object Addition extends TargetOperator
  case object Subtraction extends TargetOperator
  case object Multiplication extends TargetOperator
  case object Division extends TargetOperator
  case object Negation extends TargetOperator

  case object Truthy extends TargetOperator
  case object Equals extends TargetOperator
  case object NotEquals extends TargetOperator
  case object LessThan extends TargetOperator
  case object LessThanEquals extends TargetOperator

  case object And extends TargetOperator
  case object Or extends TargetOperator
  case object Not extends TargetOperator

  case object Concat extends TargetOperator
}
