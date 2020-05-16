package lore.compiler.definitions

import lore.compiler.ast.StmtNode

trait CallTarget extends PositionedDefinition {
  def signature: FunctionSignature
  def body: Option[StmtNode]
}
