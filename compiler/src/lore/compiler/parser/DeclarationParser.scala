package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.{DeclNode, ExprNode}
import lore.compiler.syntax.DeclNode.{GlobalVariableNode, ImportNode, ModuleNode}
import scalaz.Scalaz.ToOptionIdOps

trait DeclarationParser { _: Parser with TypeParser with BasicParsers =>
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
    val members = collectSep(nli(indentation)) {
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
}
