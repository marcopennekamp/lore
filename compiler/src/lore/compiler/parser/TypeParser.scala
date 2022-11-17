package lore.compiler.parser

import lore.compiler.syntax.TypeExprNode
import lore.compiler.syntax.TypeExprNode.{InstantiatedTypeNode, ListTypeNode, ShapeTypeNode, ShapeTypePropertyNode, SymbolTypeNode, TupleTypeNode, TypeNameNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Scalaz.ToOptionIdOps

trait TypeParser { _: Parser with BasicParsers =>
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
      val elements = collectSepWlmi(',', indentation, allowTrailing = true) {
        typeExpression(indentation)
      }
      elements.takeMinSize(2) <& character(')')
    }

    ws()
    val elements = if (character(')')) Some(Vector.empty) else nested().backtrack | standard()
    elements.map(TupleTypeNode(_, createPositionFrom(startIndex)))
  }

  def listType(indentation: Int): Option[ListTypeNode] =
    withPosition {
      surroundWlmi(character('['), character(']'), indentation) {
        typeExpression(indentation)
      }
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
        Some(collectSepWlmi(',', indentation, allowTrailing = true) {
          property
        })
      }
    }.map(ShapeTypeNode.tupled)
  }

  def enclosedType(indentation: Int): Option[TypeExprNode] =
    surroundWlmi(character('('), character(')'), indentation) {
      typeExpression(indentation)
    }

  /**
    * Parses a non-empty list of type arguments.
    */
  def typeArguments(indentation: Int): Option[Vector[TypeExprNode]] =
    surroundWlmi(character('['), character(']'), indentation) {
      collectSepWlmi(',', indentation, allowTrailing = true) {
        typeExpression(indentation)
      }.takeNonEmpty
    }
}
