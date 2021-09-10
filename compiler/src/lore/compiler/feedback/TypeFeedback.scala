package lore.compiler.feedback

import lore.compiler.syntax.TypeDeclNode

object TypeFeedback {

  case class AlreadyExists(node: TypeDeclNode) extends Feedback.Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

}
