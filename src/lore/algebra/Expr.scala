package lore.algebra

import lore.algebra.ExprTheory.ExprAlgebra
import matryoshka.Algebra

import scalaz.\/

// TODO: How to implement a nanopass compiler with this?
trait ExprTheory[ANum, ABool] {
  sealed trait Expr[T]
  sealed trait NExpr extends Expr[ANum]
  sealed trait BExpr extends Expr[ABool]
  final case class Num(value: Long) extends NExpr
  final case class Bool(value: Boolean) extends BExpr
  final case class Mul(a: NExpr, b: NExpr) extends NExpr
  final case class If(cond: BExpr, onTrue: NExpr, onFalse: NExpr) extends NExpr

  def transform[BNum, BBool](theory: ExprTheory[BNum, BBool])(expr: NExpr \/ BExpr)(fNum: NExpr => theory.NExpr)(fBool: BExpr => theory.BExpr): theory.NExpr \/ theory.BExpr = {
    def transformNum(expr: NExpr) = expr match {
      case Num(value) => fNum(Num(value))
      case Mul(a, b) => theory.Mul(fNum(a), fNum(b))
      case If(cond, t, f) => theory.If(fBool(cond), fNum(t), fNum(f))
    }

    def transformBool(expr: BExpr) = expr match {
      case Bool(value) => fBool(Bool(value))
    }

    expr.bimap(transformNum, transformBool)
  }

  def eval[BNum, BBool](alg: ExprAlgebra[BNum, BBool])(expr: NExpr): BNum = {
    val evalNum = eval(alg) _
    def evalBool(expr: BExpr): BBool = expr match {
      case Bool(value) => alg.bool(value)
    }

    expr match {
      case Num(value) => alg.num(value)
      case Mul(a, b) => alg.mul(evalNum(a))(evalNum(b))
      case If(cond, tru, fls) => alg.fIf(evalBool(cond))(evalNum(tru))(evalNum(fls))
    }
  }
}

object ExprTheory {
  trait ExprAlgebra[Num, Bool] {
    def num: Long => Num
    def bool: Boolean => Bool
    def mul: Num => Num => Num
    def fIf: Bool => Num => Num => Num
  }

  val evalAlgebra = new ExprAlgebra[Long, Boolean] {
    override def num = identity
    override def bool = identity
    override def mul = a => b => a * b
    override def fIf = cond => tru => fls => if (cond) tru else fls
  }
}

sealed trait Expr1[T]

object Expr1 {
  final case class Num[T](value: Long) extends Expr1[T]
  final case class Bool[T](value: Boolean) extends Expr1[T]
  final case class Mul[T](a: T, b: T) extends Expr1[T]
  final case class If[T](cond: T, onTrue: T, onFalse: T) extends Expr1[T]


  implicit val exprFunctor = new scalaz.Functor[Expr1] {
    override def map[A, B](fa: Expr1[A])(f: A => B) = fa match {
      case Num(value) => Num[B](value)
      case Bool(value) => Bool[B](value)
      case Mul(a, b) => Mul[B](f(a), f(b))
      case If(cond, tru, fls) => If[B](f(cond), f(tru), f(fls))
    }
  }

  val eval: Algebra[Expr1, Long] = {
    case Num(x) => x
    case Bool(b) => if (b) 1 else 0
    case Mul(x, y) => x * y
    case If(cond, onTrue, onFalse) => if (cond == 1) onTrue else onFalse
  }
}
