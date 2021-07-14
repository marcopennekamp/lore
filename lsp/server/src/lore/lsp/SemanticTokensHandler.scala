package lore.lsp

import lore.compiler.build.SourceFiles
import lore.compiler.core.Position
import lore.compiler.feedback.{LambdaReporter, Reporter}
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.syntax.visitor.CombiningNodeVisitor
import lore.compiler.syntax.{DeclNode, Node}
import lore.compiler.utils.Timer.timed
import lore.lsp.index.GlobalIndex
import lore.lsp.utils.PositionUtil
import org.eclipse.lsp4j
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.{SemanticTokenTypes, SemanticTokensLegend}
import scalaz.Id.Id

import java.nio.file.Path
import scala.jdk.CollectionConverters._

object SemanticTokensHandler {

  val legend: SemanticTokensLegend = {
    import SemanticTokenTypes._
    new SemanticTokensLegend(
      Vector(Type, Interface, Struct, TypeParameter, Parameter, Variable, Property, Function, Keyword, String, Number, Operator).asJava,
      Vector().asJava,
    )
  }

  /**
    * Parses the given file and produces semantic tokens for all nodes that should be highlighted.
    *
    * This won't perform well, but will be especially useful as long as the syntax is still changing to have a single
    * point of truth. At this stage, it would be problematic to have to maintain two different grammars (fastparse, and
    * TextMate for VSCode).
    *
    * If the performance holds up (or if we can make incremental parsing work), this might actually be the preferred
    * method of supporting syntax highlighting since we can change colors based on context information. For example,
    * taking a SimpleCallNode, a multi-function name could be differently colored in comparison to a function value
    * name.
    */
  def semanticTokens(fragmentPath: Path)(implicit globalIndex: GlobalIndex, client: LanguageClient): Option[Vector[Int]] = {
    implicit val reporter: Reporter = new LambdaReporter(feedback => MessageLogger.info(feedback.toString))

    // We should keep an eye on the execution time of this command. Currently, it hovers in the range of 5-10ms, mostly
    // due to how long the parser takes. This scales with file length, so large files might become very laggy.
    // A simple optimization would be to only reparse this if the file has changed.
    timed("Creating semantic tokens", log = MessageLogger.info) {
      SourceFiles.ofFile(fragmentPath).flatMap { fragment =>
        val nodes = ParsingPhase.process(fragment)

        // Only send new highlights to the client if we can actually parse the file. Otherwise, all existing highlights
        // would get wiped.
        if (!reporter.hasErrors) {
          val highlights = nodes.flatMap(CombiningNodeVisitor.VectorApplicator(SemanticTokensVisitor).visit(_))
          Some(relativize(highlights))
        } else None
      }
    }
  }

  private case class Highlight(position: lsp4j.Position, length: Int, tokenType: String)

  private object SemanticTokensVisitor extends CombiningNodeVisitor[Vector[Highlight], Id] {
    override protected def visit(node: Node, children: Vector[Highlight]): Vector[Highlight] = {
      highlight(node) ++ children
    }

    private def highlight(node: Node): Vector[Highlight] = node match {
      case DeclNode.FunctionNode(nameNode, _, _, _, _, isAction, position) =>
        val keyword = if (isAction) "action" else "function"
        Vector(
          createHighlight(position, keyword.length, SemanticTokenTypes.Keyword),
          createHighlight(nameNode, SemanticTokenTypes.Function)
        )
      case DeclNode.ParameterNode(nameNode, _, _) => Vector(createHighlight(nameNode, SemanticTokenTypes.Parameter))
      case _ => Vector.empty
    }

    private def createHighlight(node: Node, tokenType: String): Highlight = {
      Highlight(PositionUtil.toStartPosition(node.position), node.position.length, tokenType)
    }

    private def createHighlight(startPosition: Position, length: Int, tokenType: String): Highlight = {
      Highlight(PositionUtil.toStartPosition(startPosition), length, tokenType)
    }
  }

  /**
    * Converts the highlights into the LSP's relative semantic token format.
    */
  private def relativize(highlights: Vector[Highlight]): Vector[Int] = {
    var lastLine = 0
    var lastStart = 0

    sort(highlights).flatMap { highlight =>
      val line = highlight.position.getLine
      val start = highlight.position.getCharacter

      val deltaLine = line - lastLine
      lastLine = line

      if (deltaLine > 0) {
        lastStart = 0
      }
      val deltaStart = start - lastStart
      lastStart = start

      Vector(deltaLine, deltaStart, highlight.length, legend.getTokenTypes.indexOf(highlight.tokenType), 0)
    }
  }

  private def sort(highlights: Vector[Highlight]): Vector[Highlight] = {
    highlights.sortWith { case (h1, h2) =>
      h1.position.getLine < h2.position.getLine ||
        h1.position.getLine == h2.position.getLine && h1.position.getCharacter < h2.position.getCharacter
    }
  }

}
