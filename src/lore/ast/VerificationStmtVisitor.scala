package lore.ast

/**
  * A compilation statement visitor that implements empty and combine for verifications (which is trivial).
  */
trait VerificationStmtVisitor extends CompilationStmtVisitor[Unit] {
  override def empty: Unit = ()
  override def combine(list: List[Unit]): Unit = ()
}
