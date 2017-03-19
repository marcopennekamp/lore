package lore.algebra

import matryoshka.Algebra

sealed trait Expr[A]
final case class Num[A](value: Long) extends Expr[A]
final case class Mul[A](l: A, r: A) extends Expr[A]

object Expr {
  implicit val exprFunctor = new scalaz.Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: (A) => B) = fa match{
      case Num(value) => Num[B](value)
      case Mul(l, r) => Mul(f(l), f(r))
    }
  }

  val eval: Algebra[Expr, Long] = {
    case Num(x)    => x
    case Mul(x, y) => x * y
  }
}
