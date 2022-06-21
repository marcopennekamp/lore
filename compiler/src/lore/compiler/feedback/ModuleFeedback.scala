package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.ModuleMember
import lore.compiler.syntax.DeclNode.ImportNode

object ModuleFeedback {
  case class MemberNameTaken(
    memberName: String,
    existing: ModuleMember,
    override val position: Position,
  ) extends Feedback.Error(position) {
    override def message: String = s"The name of the binding `$memberName` is already taken by another binding at" +
      s" ${existing.firstPosition}."
  }

  object Import {
    abstract class ImportError(node: ImportNode) extends Feedback.Error(node.position) {
      protected def introduction: String = s"The import of `${node.namePathNode.namePath}` cannot be resolved: "
      protected def simpleName: String = node.namePathNode.namePath.simpleName
    }

    case class TooShort(node: ImportNode) extends ImportError(node) {
      override def message: String = introduction + s"An import path must have at least two segments. Single imports" +
        s" such as `use A` are nonsensical and thus illegal."
    }

    case class NotFound(node: ImportNode, path: String) extends ImportError(node) {
      override def message: String = introduction + s"`$path` does not exist."
    }

    case class ModuleExpected(node: ImportNode, headSegment: String) extends ImportError(node) {
      override def message: String = introduction + s"`$headSegment` is not a module."
    }

    object DirectImport {
      case class CannotOverrideLocalDeclaration(node: ImportNode) extends ImportError(node) {
        override def message: String = introduction + s"`$simpleName` is already declared locally. Direct imports may" +
          s" not conflict with local declarations."
      }

      case class CannotOverrideDirectImport(node: ImportNode, existingPaths: Set[NamePath]) extends ImportError(node) {
        override def message: String = introduction + s"`$simpleName` is already imported directly from" +
          s" ${existingPaths.map(path => s"`${path.toString}`").mkString(", ")}. Direct imports may not conflict with" +
          s" other direct imports."
      }
    }
  }
}
