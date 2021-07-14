package lore.lsp

import lore.compiler.build.SourceFiles
import lore.compiler.feedback.{LambdaReporter, Reporter}
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.syntax.visitor.NodeSeeker
import lore.compiler.syntax.{DeclNode, ExprNode, Node, TypeExprNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import lore.compiler.utils.Timer.timed
import lore.lsp.index.GlobalIndex
import org.eclipse.lsp4j
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.services.LanguageClient

import java.nio.file.Path

object DefinitionHandler {

  /**
    * Finds all locations where the type or binding at the specific location in the source is defined. The current
    * implementation can only list global declarations and disregards local scopes and shadowing entirely.
    */
  def definition(fragmentPath: Path, position: lsp4j.Position)(implicit globalIndex: GlobalIndex, client: LanguageClient): Option[Vector[Location]] = {
    implicit val reporter: Reporter = new LambdaReporter(feedback => MessageLogger.info(feedback.toString))

    // We should keep an eye on the execution time of this command. Currently, it hovers in the range of 5-10ms, mostly
    // due to how long the parser takes. This scales with file length, so large files might become very laggy.
    // A simple optimization would be to only reparse this if the file has changed.
    timed("Finding definitions", log = MessageLogger.info) {
      SourceFiles.ofFile(fragmentPath).flatMap { fragment =>
        val nodes = ParsingPhase.process(fragment)
        val target = NodeSeeker.Target(position.getLine + 1, position.getCharacter + 1)
        val usageSeeker = UsageSeeker(target)

        nodes.firstDefined(NodeSeeker.visit(usageSeeker)) match {
          case Some(usage) => usage.kind match {
            case UsageType => globalIndex.getTypeDeclaration(usage.name).map(_.locations)
            case UsageBinding => globalIndex.getBindingDeclaration(usage.name).map(_.locations)
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
        case DeclNode.FunctionNode(name, _, _, _, _, _) => result(node, name, UsageBinding)

        case TypeExprNode.IdentifierNode(name, _) => result(node, name, UsageType)

        case ExprNode.VariableNode(name, _) => result(node, name, UsageBinding)
        case ExprNode.FixedFunctionNode(name, _, _) => result(node, name, UsageBinding)
        case ExprNode.ObjectMapNode(structName, _, _) => result(node, structName, UsageType)
        case ExprNode.SimpleCallNode(name, _, _) => result(node, name, UsageBinding)

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
