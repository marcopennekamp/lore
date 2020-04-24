package lore.ast.visitor

/**
  * A pass-on statement visitor that implements empty and combine for verifications (which is trivial).
  */
trait VerificationStmtVisitor extends PassOnStmtVisitor[Unit] {
  override def empty: Unit = ()
  override def combine(list: List[Unit]): Unit = ()
}
