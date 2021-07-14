package lore.lsp

import lore.compiler.feedback.Reporter
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.syntax.visitor.NodeSeeker
import lore.compiler.syntax.{DeclNode, ExprNode, Node, TypeExprNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import lore.compiler.utils.Timer.timed
import org.eclipse.lsp4j
import org.eclipse.lsp4j.Location

object DefinitionHandler {

  /**
    * Finds all locations where the type or binding at the specific location in the source is defined. The current
    * implementation can only list global declarations and disregards local scopes and shadowing entirely.
    */
  def definition(fragmentUri: String, position: lsp4j.Position)(implicit context: LanguageServerContext): Option[Vector[Location]] = {
    // We should keep an eye on the execution time of this command. Currently, it hovers in the range of 5-10ms, mostly
    // due to how long the parser takes. This scales with file length, so large files might become very laggy.
    // A simple optimization would be to only reparse this if the file has changed.
    timed("Finding definitions", log = MessageLogger.info) {
      context.fragmentManager.get(fragmentUri).flatMap { fragment =>
        implicit val reporter: Reporter = MessageLogger.freshReporter
        val nodes = ParsingPhase.process(fragment)
        val target = NodeSeeker.Target(position.getLine + 1, position.getCharacter + 1)
        val usageSeeker = UsageSeeker(target)

        nodes.firstDefined(NodeSeeker.visit(usageSeeker)) match {
          case Some(usage) => usage.kind match {
            case UsageType => context.globalIndex.getTypeDeclaration(usage.name).map(_.locations)
            case UsageBinding => context.globalIndex.getBindingDeclaration(usage.name).map(_.locations)
          }
          case None => None
        }
      }
    }
  }

  private sealed trait UsageKind
  private case object UsageType extends UsageKind
  private case object UsageBinding extends UsageKind

  private case class DefinitionUsage(name: String, kind: UsageKind)

  private case class UsageSeeker(target: NodeSeeker.Target) extends NodeSeeker[DefinitionUsage](target) {

    override protected def result(node: Node): Option[DefinitionUsage] = {
      node match {
        // It's useful to be able to use "go to definition" on a function definition to get a list of all locations
        // where the multi-functions is defined.
        case DeclNode.FunctionNode(nameNode, _, _, _, _, _, _) => result(node, nameNode.value, UsageBinding)

        case TypeExprNode.TypeNameNode(name, _) => result(node, name, UsageType)

        case ExprNode.VariableNode(name, _) => result(node, name, UsageBinding)
        case ExprNode.FixedFunctionNode(nameNode, _, _) => result(node, nameNode.value, UsageBinding)
        case ExprNode.ObjectMapNode(nameNode, _, _) => result(node, nameNode.value, UsageType)
        case ExprNode.SimpleCallNode(nameNode, _, _) => result(node, nameNode.value, UsageBinding)

        case _ => None
      }
    }

    /**
      * We have to check that the node was actually selected by name. For example, a simple call node's position ranges
      * from the first character of the function name to the end of the argument list. But when a user selects an
      * argument, we don't want to jump to the definition of the function. For example:
      *
      * {{{
      * foobar(1, abc)
      * }}}
      *
      * If the user selects "foobar", we want to jump to the definition of foobar. If they select "abc", we want to
      * jump to the definition of abc. But if they select "1", we don't want to jump to anything. Hence, we have to
      * ensure that any position after the name is disregarded.
      */
    private def result(node: Node, name: String, kind: UsageKind): Option[DefinitionUsage] = {
      val position = node.position
      val endColumn = position.startColumn + name.length
      if (target.containedIn(position.startLine, position.startColumn, position.startLine, endColumn)) {
        Some(DefinitionUsage(name, kind))
      } else None
    }

  }

}
