package lore.compiler.definitions

import lore.compiler.ast.StmtNode
import lore.types.Type

trait InternalCallTarget extends CallTarget with PositionedDefinition {
  def signature: FunctionSignature
  def body: Option[StmtNode]
  def typeScope: TypeScope

  override def name: String = signature.name
  override def outputType: Type = signature.outputType
}
