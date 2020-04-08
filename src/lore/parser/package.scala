package lore

package object parser {
  def aggregate[Expr, A](node: List[Expr] => A)(in: (Expr, Seq[Expr])): A = {
    val (expression1, expressions) = in
    node(expression1 +: expressions.toList)
  }
}
