package lore.compiler.parser

import fastparse.ParserInput
import lore.compiler.core.{Fragment, Position}
import lore.compiler.syntax.DeclNode.{GlobalVariableNode, ImportNode, ModuleNode}
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.TypeExprNode._
import lore.compiler.syntax.{DeclNode, ExprNode, TypeExprNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Scalaz.ToOptionIdOps

object LoreParser {
  /**
    * TODO (syntax): Document.
    */
  def parse(input: String)(implicit fragment: Fragment): Option[ModuleNode] = new LoreParser(input).parse()

  def main(args: Array[String]): Unit = {
    val source =
      """let abc: String = TODO
        |
        |module Hello
        |
        |  module World
        |     let me: [Int] = TODO
        |
        |  module Meme
        |    let abc: String = TODO
        |module Dead
        |
        |
        |    let abcd: Alphabet = TODO
        |    module Holy
        |
        |""".stripMargin
    parse(source)(Fragment("test_fragment", ParserInput.fromString(source)))
  }
}

// TODO (syntax): Implement syntax error reporting with `reporter` and good error messages...
//                However, we will need to collect errors in a result structure first (replacing `Option`) to support
//                error backtracking, which will require a minor or major parser rewrite because errors will have to be
//                concatenated.

/**
  * Implementation conventions:
  *   - Failed parsers may be stuck at an advanced [[Parser.offset]] and must be backtracked explicitly with
  *     [[Parser.BooleanActionExtension.backtrack]] or [[Parser.OptionActionExtension.backtrack]]. Parsers that are
  *     marked as [[Parser.OffsetConservative]] don't need to be backtracked even if they fail. This convention
  *     improves performance by avoiding lots of saved offsets.
  */
private class LoreParser(override val input: String)(override implicit val fragment: Fragment) extends Parser with NameParser {

  def parse(): Option[ModuleNode] = {
    parseFragment()
    // TODO (syntax): Only return `Some` if `offset` is at EOF.
  }

  def parseFragment(): Option[ModuleNode] = {
    // TODO (syntax): The top module declaration needs some special handling, as a `module X` declaration is only a top
    //                module if it has no body. So we have to get a `module()`, check if it has a body, and if it has,
    //                it's actually not a top module but the first module member!
    val startIndex = offset
    blankLines()
    println(s"Consumed whitespace and blank lines until offset $offset.")
    val result = moduleBody(0).map { case (imports, members) =>
      ModuleNode(NamePathNode.empty, atRoot = false, imports, members, createPositionFrom(startIndex))
    }
    println(s"End position: ${fragment.input.prettyIndex(offset)}")
    println(s"Peek char: `$peek`")
    println(s"Peek line: ${input.substring(offset).takeWhile(_ != '\n')}")

    wl()

    println(s"Parser result: $result")

    result.filter { _ =>
      if (peek != EOF) {
        println("ERROR: Parser did not read the whole file!")
        false
      } else true
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modules.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (syntax): Parse `@root`.
  def module(indentation: Int): Option[ModuleNode] = {
    val startIndex = offset
    if (!word("module") || !ws()) return None

    val moduleName = namePath().getOrElse(return None)
    val (imports, members) = indent(indentation)
      .flatMap(bodyIndentation => moduleBody(bodyIndentation))
      .getOrElse((Vector.empty, Vector.empty))

    ModuleNode(moduleName, atRoot = false, imports, members, createPositionFrom(startIndex)).some
  }

  // TODO (syntax): Parse imports.
  def moduleBody(indentation: Int): Option[(Vector[ImportNode], Vector[DeclNode])] = {
    println(s"Module body indentation: $indentation")
    val members = collectSep(nli(indentation).backtrack) {
      // TODO (syntax): This optimization needs to be taken very carefully. For example, an `@root` module will start
      //                with `@`, not `m`. If any top-level declaration other than a module can start with the letter
      //                `m`, this must be changed. (For example by falling back on the default case if any of the
      //                optimistic cases fail.) Note that `proc` will suffer from this if we add a `private` keyword.
      peek match {
        case 'm' => module(indentation)
        case 'l' => globalVariable(indentation)
        //case 'f' => function(indentation)
        //case 'p' => procedure(indentation)
        //case 't' => type alias or trait (differentiate by peek(2) == 'y' or 'r')
        //case 's' => struct or struct alias or spec (differentiate by peek(2) == 't' or 'p')
        //case 'o' => object or object alias
        case _ => module(indentation).backtrack | globalVariable(indentation)
      }
    }
    (Vector.empty, members).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def globalVariable(indentation: Int): Option[GlobalVariableNode] = {
    val startIndex = offset
    if (!word("let") || !ws()) return None

    val variableName = name().getOrElse(return None)
    ws()
    val variableType = typing(indentation).getOrElse(return None)
    ws()

    if (!character('=') <* ws() || !word("TODO")) return None

    GlobalVariableNode(
      variableName,
      variableType,
      ExprNode.TupleNode(Vector.empty, Position.unknown),
      createPositionFrom(startIndex),
    ).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Functions and domains.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def function() = ???

  def procedure() = ???

  def signature() = ???

  def domain() = ???

  def parameter() = ???

  def where() = ???

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def typing(indentation: Int): Option[TypeExprNode] = {
    if (!character(':')) return None
    ws()
    typeExpression(indentation)
  }

  def typeExpression(indentation: Int): Option[TypeExprNode] = {
    // TODO (syntax): PrecedenceParser bla bla
    typeAtom(indentation)
  }

  def typeAtom(indentation: Int): Option[TypeExprNode] = peek match {
    case '#' => symbolType()
    case '(' => tupleType(indentation).backtrack | enclosedType(indentation)
    case '[' => listType(indentation)
    case '%' => shapeType(indentation)
    case _ => instantiatedType(indentation).backtrack | namedType()
  }

  def symbolType(): Option[SymbolTypeNode] = {
    withPosition(character('#') &> identifier()).map(SymbolTypeNode.tupled)
  }

  def namedType(): Option[TypeNameNode] = withPosition(typeNamePath()).map(TypeNameNode.tupled)

  def instantiatedType(indentation: Int): Option[InstantiatedTypeNode] = {
    val startOffset = offset

    val typeName = namedType().getOrElse(return None)
    ws()
    val typeArgs = typeArguments(indentation).getOrElse(return None)

    InstantiatedTypeNode(typeName, typeArgs, createPositionFrom(startOffset)).some
  }

  /**
    * The parser for tuple types doesn't support 1-tuples with a syntax `(A)`, because this would clash with the
    * `enclosedType` parser. However, we still want to be able to parse function types that work on single tuple
    * arguments. The syntax `(Int, Int) => Int` would create a function type with two arguments, so we need a special
    * syntax. The solution is to parse `((Int, Int))` as a *nested* tuple.
    */
  def tupleType(indentation: Int): Option[TupleTypeNode] = {
    val startIndex = offset
    if (!character('(')) return None

    def nested() = {
      (tupleType(indentation).map(Vector(_)) <* wlmi(indentation)) <& character(')')
    }

    def standard() = {
      val elements = collectSepWlmi(',', indentation, allowTrailing = true) { typeExpression(indentation) }
      elements.takeMinSize(2) <& character(')')
    }

    ws()
    val elements = if (character(')')) Some(Vector.empty) else nested().backtrack | standard()
    elements.map(TupleTypeNode(_, createPositionFrom(startIndex)))
  }

  def listType(indentation: Int): Option[ListTypeNode] =
    withPosition {
      surroundWlmi(character('['), character(']'), indentation) { typeExpression(indentation) }
    }.map(ListTypeNode.tupled)

  def shapeType(indentation: Int): Option[ShapeTypeNode] = {
    def property: Option[ShapeTypePropertyNode] = {
      val startOffset = offset
      val propertyName = name().getOrElse(return None)
      ws()
      val propertyType = typing(indentation).getOrElse(return None)
      ShapeTypePropertyNode(propertyName, propertyType, createPositionFrom(startOffset)).some
    }

    withPosition {
      surroundWlmi(word("%{"), character('}'), indentation) {
        Some(collectSepWlmi(',', indentation, allowTrailing = true) { property })
      }
    }.map(ShapeTypeNode.tupled)
  }

  def enclosedType(indentation: Int): Option[TypeExprNode] =
    surroundWlmi(character('('), character(')'), indentation) { typeExpression(indentation) }

  /**
    * Parses a non-empty list of type arguments.
    */
  def typeArguments(indentation: Int): Option[Vector[TypeExprNode]] =
    surroundWlmi(character('['), character(']'), indentation) {
      collectSepWlmi(',', indentation, allowTrailing = true) { typeExpression(indentation) }.takeNonEmpty
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Values.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def int(): Option[Long] = {
    ???
  }

  def real(): Option[Double] = {
    ???
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Indentation.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Attempts to open a new indentation block on the next non-blank line. Returns the next line's indentation if it's
    * higher than `parentIndentation`. `indent` expects to be at the end of the line; if instead another token is
    * encountered, `indent` will return `None`.
    */
  def indent(parentIndentation: Int): Option[Int] = {
    if (!nextNonBlankLine()) return None

    val startOffset = offset
    chars(' ')
    val indentation = offset - startOffset
    println(s"Branch indentation ($parentIndentation) at ${fragment.input.prettyIndex(startOffset)}: $indentation indented")
    if (parentIndentation < indentation) Some(indentation) else None
  }

  /**
    * Consumes whitespace until the first non-blank line and checks that the current indentation is equal to the given
    * indentation. The parser succeeds only if a newline is encountered.
    */
  def nli(indentation: Int): Boolean = {
    if (!nextNonBlankLine()) {
      println(s"Invalid exact indentation ($indentation) at ${fragment.input.prettyIndex(offset)}: no next non-blank line found")
      return false
    }

    val count = countIndentation()
    if (indentation == count) true
    else {
      println(s"Invalid exact indentation ($indentation) at ${fragment.input.prettyIndex(offset)}: $indentation != $count")
      false
    }
  }

  /**
    * Like `wl` with the following difference: if a new line is encountered, the indentation of the first non-blank
    * line must be greater than or equal to the given indentation. `wlmi` may just consume whitespace on the same line
    * until it encounters the next token; the indentation is only checked if a newline is encountered. Returns `true`
    * if at least one character was consumed and the indentation is valid.
    *
    * `wlmi` should be used with statements that need to have some minimum indentation, for example with types such as:
    *
    * <pre>
    * val abc: Either[
    *   String,
    *   Int,
    * ] =
    *   Left("String")
    * </pre>
    *
    * This is a matter of enforcing consistency for indentation-sensitive expressions that might follow the type, even
    * if the type expression itself could be parsed without paying attention to indentation. In the example above, if
    * `]` wasn't forced to at least be aligned with `val abc`, the block starter `=` would be misaligned.
    */
  def wlmi(indentation: Int): Boolean =
    // No need to backtrack from `nextNonBlankLine` because it'll have consumed only whitespace anyway.
    if (nextNonBlankLine()) indentation <= countIndentation()
    else ws() // TODO (syntax): Do we even need `ws()` here or does the side effect from `nextNonBlankLine()` suffice?

  /**
    * Like `wl` with the following difference: if a new line is encountered, the indentation of the first non-blank
    * line must be greater than the given indentation. `wlgi` may just consume whitespace on the same line until it
    * encounters the next token; the indentation is only checked if a newline is encountered. Returns `true` if at
    * least one character was consumed and the indentation is valid.
    *
    * `wlgi` should be used with multi-line expressions that don't open a new indentation block, such as:
    *
    * <pre>
    * val a = 1 + 2 +
    *   3 + 4
    * </pre>
    *
    * The `3 + 4` belongs to the larger expression `1 + 2 + 3 + 4`. `3 + 4` is not contained in a block, but its
    * indentation must still be checked so that it isn't part of the previous block.
    */
  def wlgi(indentation: Int): Boolean =
    // No need to backtrack from `nextNonBlankLine` because it'll have consumed only whitespace anyway.
    if (nextNonBlankLine()) indentation < countIndentation()
    else ws() // TODO (syntax): Do we even need `ws()` here or does the side effect from `nextNonBlankLine()` suffice?

  private def countIndentation(): Int = {
    val startOffset = offset
    chars(' ')
    offset - startOffset
  }

  def collectSepWlmi[A](separator: Char, indentation: Int, allowTrailing: Boolean = false)(get: => Option[A]): Vector[A] =
    // TODO (syntax): Is backtracking needed here? I think in some edge cases it is, where newlines shouldn't be
    //                consumed by `wlmi` if `separator` cannot be found...
    collectSep((wlmi(indentation) *> character(separator) <* wlmi(indentation)).backtrack, allowTrailing)(get)

  def surroundWlmi[A](left: => Boolean, right: => Boolean, indentation: Int)(get: => Option[A]): Option[A] =
    // No backtracking needed because `left` and `right` neatly close off the whitespaces on either side.
    surround(left <* wlmi(indentation), wlmi(indentation) *> right)(get)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Whitespace and comments.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Consumes whitespace except for newlines, returning `true` if at least one character was consumed.
    */
  @OffsetConservative
  def ws(): Boolean = repeat(spacesOrTabs() || comment())

  /**
    * Consumes whitespace including newlines, returning `true` if at least one character was consumed.
    */
  @OffsetConservative
  def wl(): Boolean = repeat(ws() || newline())

  @OffsetConservative
  def spacesOrTabs(): Boolean = charsWhile(c => c == ' ' || c == '\t')

  @OffsetConservative
  def newline(): Boolean = character('\n') || word("\r\n")

  @OffsetConservative
  def blankLines(): Boolean = repeat((ws() *> newline()).backtrack)

  /**
    * Moves the cursor to the next non-blank line.
    */
  def nextNonBlankLine(): Boolean = {
    if (ws() *> newline()) {
      // Ignore subsequent blank lines.
      blankLines()
      true
    } else false
  }

  // TODO (syntax): Support block comments.
  @OffsetConservative
  def comment(): Boolean = lineComment()

  @OffsetConservative
  def lineComment(): Boolean = {
    if (word("--")) {
      while (peek != '\n' && (peek != '\r' || peek(2) != '\n')) {
        consume()
      }
      true
    } else false
  }
}
