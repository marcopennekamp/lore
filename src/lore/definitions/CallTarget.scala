package lore.definitions

import lore.ast.StmtNode

trait CallTarget extends PositionedDefinition {
  def signature: FunctionSignature
  def body: Option[StmtNode]
}
