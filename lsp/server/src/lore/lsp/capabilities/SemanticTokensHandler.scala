package lore.lsp.capabilities

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.syntax.Node.Index
import lore.compiler.syntax.visitor.CombiningNodeVisitor
import lore.compiler.syntax._
import lore.compiler.utils.Timer.timed
import lore.lsp.LanguageServerContext
import lore.lsp.utils.{MessageLogger, PositionUtil}
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{SemanticTokenTypes, SemanticTokensLegend}
import scalaz.Id.Id

import scala.jdk.CollectionConverters._

object SemanticTokensHandler {

  val legend: SemanticTokensLegend = {
    import SemanticTokenTypes._
    new SemanticTokensLegend(
      Vector(Type, Interface, Struct, TypeParameter, Parameter, Variable, Property, EnumMember, Function, Keyword, String, Number, Operator).asJava,
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
  def semanticTokens(fragmentUri: String)(implicit context: LanguageServerContext): Option[Vector[Int]] = {
    // We should keep an eye on the execution time of this command. Currently, it hovers in the range of 5-10ms, mostly
    // due to how long the parser takes. This scales with file length, so large files might become very laggy.
    // A simple optimization would be to only reparse this if the file has changed.
    timed("Creating semantic tokens", log = MessageLogger.info) {
      context.fragmentManager.get(fragmentUri).flatMap { fragment =>
        implicit val reporter: Reporter = MessageLogger.freshReporter
        val nodes = ParsingPhase.process(fragment)

        // Only send new highlights to the client if we can actually parse the file. Otherwise, all existing highlights
        // would get wiped.
        if (!reporter.hasErrors) {
          val highlights = nodes.flatMap(CombiningNodeVisitor.VectorApplicator(new SemanticTokensVisitor).visit(_))
          Some(relativize(highlights))
        } else None
      }
    }
  }

  private case class Highlight(position: lsp4j.Position, length: Int, tokenType: String)

  private class SemanticTokensVisitor(implicit context: LanguageServerContext) extends CombiningNodeVisitor[Vector[Highlight], Id] {
    override protected def visit(node: Node, children: Vector[Highlight]): Vector[Highlight] = {
      highlight(node) ++ children
    }

    private def highlight(node: Node): Vector[Highlight] = node match {
      case DeclNode.FunctionNode(nameNode, _, outputType, typeVariables, _, _, position) =>
        val whereHighlight = typeVariables.headOption.map(
          tv1 => createKeywordHighlight(outputType.position, tv1.position)
        ).toVector

        Vector(
          createKeywordHighlight(position.startIndex, nameNode.position),
          createHighlight(nameNode, SemanticTokenTypes.Function),
        ) ++ whereHighlight

      case DeclNode.ParameterNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Parameter)
      case DeclNode.TypeVariableNode(nameNode, _, _, _) => singleHighlight(nameNode, SemanticTokenTypes.TypeParameter)

      case TypeDeclNode.AliasNode(nameNode, _, position) => Vector(
        createKeywordHighlight(position.startIndex, nameNode.position),
        createHighlight(nameNode, SemanticTokenTypes.Type),
      )

      case TypeDeclNode.TraitNode(nameNode, extended, position) =>
        val extendedHighlight = extended.headOption.map(
          tpe => createKeywordHighlight(nameNode.position, tpe.position)
        ).toVector

        Vector(
          createKeywordHighlight(position.startIndex, nameNode.position),
          createHighlight(nameNode, SemanticTokenTypes.Interface),
        ) ++ extendedHighlight

      case TypeDeclNode.StructNode(nameNode, extended, _, position) =>
        val extendedHighlight = extended.headOption.map(
          tpe => createKeywordHighlight(nameNode.position, tpe.position)
        ).toVector

        Vector(
          createKeywordHighlight(position.startIndex, nameNode.position),
          createHighlight(nameNode, SemanticTokenTypes.Struct),
        ) ++ extendedHighlight

      case TypeDeclNode.PropertyNode(nameNode, _, _, _, _, position) => Vector(
        // This colors both `open` and `mut`, if applicable.
        createKeywordHighlight(position.startIndex, nameNode.position),
        createHighlight(nameNode, SemanticTokenTypes.Property),
      )

      case TypeExprNode.TypeNameNode(_, _) => singleHighlight(node, SemanticTokenTypes.Type)
      case TypeExprNode.ShapePropertyNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Property)
      case TypeExprNode.SymbolNode(_, _) => singleHighlight(node, SemanticTokenTypes.EnumMember)

      case TopLevelExprNode.ReturnNode(expr, position) => Vector(createKeywordHighlight(position.startIndex, expr.position))
      case TopLevelExprNode.VariableDeclarationNode(nameNode, _, _, _, position) => Vector(
        createKeywordHighlight(position.startIndex, nameNode.position),
        createHighlight(nameNode, SemanticTokenTypes.Variable),
      )

      case ExprNode.VariableNode(_, _) => singleHighlight(node, SemanticTokenTypes.Variable)

      case ExprNode.RealLiteralNode(_, _) => singleHighlight(node, SemanticTokenTypes.Number)
      case ExprNode.IntLiteralNode(_, _) => singleHighlight(node, SemanticTokenTypes.Number)
      case ExprNode.BoolLiteralNode(_, _) => singleHighlight(node, SemanticTokenTypes.Keyword)

      case ExprNode.StringLiteralNode(_, _) => singleHighlight(node, SemanticTokenTypes.String)
      case ExprNode.ConcatenationNode(_, position) =>
        // We have to highlight the single quotes!
        val start = PositionUtil.fromStartPosition(position)
        val end = PositionUtil.fromEndPosition(position)
        end.setCharacter(end.getCharacter - 1)
        Vector(
          createHighlight(start, 1, SemanticTokenTypes.String),
          createHighlight(end, 1, SemanticTokenTypes.String),
        )

      case ExprNode.AnonymousFunctionParameterNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Variable)
      case ExprNode.FixedFunctionNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Function)

      case ExprNode.MemberAccessNode(_, nameNode, _) => singleHighlight(nameNode, SemanticTokenTypes.Property)
      case ExprNode.ObjectMapNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Struct)
      case ExprNode.ObjectEntryNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Property)

      case ExprNode.ShapeValuePropertyNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Property)

      case ExprNode.SymbolValueNode(_, _) => singleHighlight(node, SemanticTokenTypes.EnumMember)

      case ExprNode.SimpleCallNode(nameNode, _, _) =>
        context.globalIndex.getBindingDeclaration(nameNode.value) match {
          case Some(_) => singleHighlight(nameNode, SemanticTokenTypes.Function)
          case None => context.globalIndex.getTypeDeclaration(nameNode.value) match {
            case Some(_) => singleHighlight(nameNode, SemanticTokenTypes.Struct)
            case None => singleHighlight(nameNode, SemanticTokenTypes.Variable)
          }
        }
      case ExprNode.DynamicCallNode(_, _, position) => Vector(createKeywordHighlight(position, "dynamic".length))

      case ExprNode.IfElseNode(_, onTrue, onFalse, position) =>
        val elseHighlight = onFalse.map(onFalse => createKeywordHighlight(onTrue.position, onFalse.position)).toVector
        Vector(createKeywordHighlight(position, "if".length)) ++ elseHighlight

      case ExprNode.WhileNode(_, _, position) => Vector(createKeywordHighlight(position, "while".length))
      case ExprNode.ForNode(_, _, position) => Vector(createKeywordHighlight(position, "for".length))
      case ExprNode.ExtractorNode(nameNode, _, _) => singleHighlight(nameNode, SemanticTokenTypes.Variable)

      case _ => Vector.empty
    }

    private def createHighlight(node: Node, tokenType: String): Highlight = {
      Highlight(PositionUtil.fromStartPosition(node.position), node.position.length, tokenType)
    }

    private def singleHighlight(node: Node, tokenType: String): Vector[Highlight] = Vector(createHighlight(node, tokenType))

    private def createHighlight(startPosition: lsp4j.Position, length: Int, tokenType: String): Highlight = {
      Highlight(startPosition, length, tokenType)
    }

    /**
      * Creates a keyword highlight between the given positions, taking the end index of `previous` and the start index
      * of `next`.
      */
    private def createKeywordHighlight(previous: Position, next: Position): Highlight = {
      createKeywordHighlight(previous.endIndex, next)
    }

    /**
      * Creates a keyword highlight from `startIndex` to the start index of `next`.
      *
      * In general, we can't be sure where keywords are placed exactly, because keyword positions aren't kept as nodes.
      * However, we can guess the position from the given parameters. At worst we will then highlight a few spaces,
      * which doesn't actually make a visual difference.
      */
    private def createKeywordHighlight(startIndex: Index, next: Position): Highlight = {
      val position = Position(next.fragment, startIndex, next.startIndex)
      createHighlight(PositionUtil.fromStartPosition(position), position.length, SemanticTokenTypes.Keyword)
    }

    /**
      * Creates a keyword highlight from `startIndex` with the given length.
      */
    private def createKeywordHighlight(startPosition: Position, length: Int): Highlight = {
      createHighlight(PositionUtil.fromStartPosition(startPosition), length, SemanticTokenTypes.Keyword)
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
