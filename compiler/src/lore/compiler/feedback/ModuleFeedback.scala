package lore.compiler.feedback

import lore.compiler.core.Positioned
import lore.compiler.semantics.definitions.BindingDefinition
import lore.compiler.syntax.DeclNode.ImportNode

object ModuleFeedback {

  case class MemberNameTaken(
    memberName: String,
    existing: BindingDefinition,
    positioned: Positioned,
  ) extends Feedback.Error(positioned) {
    override def message: String = s"The name of the binding `$memberName` is already taken by another binding at" +
      s" ${existing.position}."
  }

  abstract class ImportError(node: ImportNode) extends Feedback.Error(node.position) {
    protected def introduction: String = s"The import of `${node.namePathNode.namePath}` cannot be resolved: "
    protected def simpleName: String = node.namePathNode.namePath.simpleName
  }

  object Import {
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
  }

  object DirectImport {
    case class CannotOverrideLocalDeclaration(node: ImportNode) extends ImportError(node) {
      override def message: String = introduction + s"`$simpleName` is already declared locally. Direct imports may" +
        s" not conflict with local declarations."
    }

    case class CannotOverrideDirectImport(
      node: ImportNode,
      existingMembers: Iterable[BindingDefinition],
    ) extends ImportError(node) {
      override def message: String = introduction + s"`$simpleName` is already imported directly from" +
        s" ${existingMembers.map(member => s"`${member.name.toString}`").mkString(", ")}. Direct imports may not" +
        s" conflict with other direct imports."
    }
  }

}
