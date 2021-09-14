package lore.compiler.feedback

import lore.compiler.semantics.NamePath
import lore.compiler.syntax.DeclNode.ImportNode

object ModuleFeedback {
  object Import {
    case class TooShort(node: ImportNode) extends Feedback.Error(node) {
      override def message: String = s"An import path must have at least two segments. Single imports such as `use A` " +
        s" are nonsensical and thus illegal."
    }

    case class UnresolvedHeadSegment(node: ImportNode, headSegment: String) extends Feedback.Error(node) {
      override def message: String = s"The import of ${node.namePathNode.namePath} cannot be resolved:" +
        s" $headSegment is not a type or binding."
    }

    case class NotFound(node: ImportNode, absolutePath: NamePath) extends Feedback.Error(node) {
      override def message: String = s"The import of ${node.namePathNode.namePath} cannot be resolved:" +
        s" the import leads to a member $absolutePath, which does not exist."
    }
  }
}
