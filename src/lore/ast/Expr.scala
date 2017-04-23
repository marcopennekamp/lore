package lore.ast

import scalaz.\/

sealed trait Expr
sealed trait NumExpr extends Expr
sealed trait BoolExpr extends Expr
final case class Num(value: Long) extends NumExpr
final case class Bool(value: Boolean) extends BoolExpr
final case class Mul(a: NumExpr, b: NumExpr) extends NumExpr
final case class Branch(cond: BoolExpr, onTrue: NumExpr, onFalse: NumExpr) extends NumExpr

trait ExprAlgebra[Num, Bool] {
  def num: Long => Num
  def bool: Boolean => Bool
  def mul: Num => Num => Num
  def branch: Bool => Num => Num => Num
}

object ExprAlgebra {
  def evaluateNumExpr[N, B](algebra: ExprAlgebra[N, B])(expr: NumExpr): N = {
    val toNum = evaluateNumExpr(algebra) _
    val toBool = evaluateBoolExpr(algebra) _
    expr match {
      case Num(a) => algebra.num(a)
      case Mul(a, b) => algebra.mul(toNum(a))(toNum(b))
      case Branch(flag, onTrue, onFalse) => algebra.branch(toBool(flag))(toNum(onTrue))(toNum(onFalse))
    }
  }

  def evaluateBoolExpr[N, B](algebra: ExprAlgebra[N, B])(expr: BoolExpr): B = expr match {
    case Bool(b) => algebra.bool(b)
  }

  def evaluate[N, B](algebra: ExprAlgebra[N, B])(expr: Expr): N \/ B = expr match {
    case e: NumExpr => \/.left(evaluateNumExpr(algebra)(e))
    case e: BoolExpr => \/.right(evaluateBoolExpr(algebra)(e))
  }

  val evalAlgebra = new ExprAlgebra[Long, Boolean] {
    override def num = identity
    override def bool = identity
    override def mul = a => b => a * b
    override def branch = cond => tru => fls => if (cond) tru else fls
  }
}
