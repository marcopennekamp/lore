package lore.compiler.feedback

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.ModuleDefinition
import lore.compiler.syntax.DeclNode.ImportNode

object ModuleFeedback {
  case class NameTaken(module: ModuleDefinition) extends Feedback.Error(module.positions.head) {
    override def message: String = s"The name of the module ${module.name} is already taken by a global variable or" +
      s" multi-function. Modules, global variables, and multi-functions may not share names."
  }

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
        s" $absolutePath does not exist."
    }

    object Wildcard {
      case class TooShort(node: ImportNode) extends Feedback.Error(node) {
        override def message: String = "A wildcard import path must have at least one segment. An import `use _` is" +
          " nonsensical and thus illegal."
      }

      case class NotFound(node: ImportNode, absolutePath: NamePath) extends Feedback.Error(node) {
        override def message: String = s"This wildcard import cannot be resolved: $absolutePath does not exist or is" +
          s" not a module."
      }
    }
  }
}
