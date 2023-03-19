package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.parser.PrecedenceParser.XaryOperator
import lore.compiler.syntax.TypeExprNode._
import lore.compiler.syntax._

trait TypeParser { _: Parser with PrecedenceParser with NameParser =>
  def typing(): Result[TypeExprNode] = {
    if (!consumeIf[TkColon]) {
      // TODO (syntax): Report error: Typing colon expected.
      return Failure
    }
    typeExpression()
  }

  def typeExpression(): Result[TypeExprNode] = {
    def isOperator(token: Token) = token match {
      case TkTypeOr(_) | TkTypeAnd(_) | TkArrow(_) => true
      case _ => false
    }

    def operator = {
      consume() match {
        case TkTypeOr(_) => XaryOperator[TypeExprNode](1, TypeExprNode.SumTypeNode).success
        case TkTypeAnd(_) => XaryOperator[TypeExprNode](2, TypeExprNode.IntersectionTypeNode).success
        case TkArrow(_) => XaryOperator[TypeExprNode](3, TypeExprNode.xaryFunction).success
        case _ => Failure
      }
    }

    parseOperationWithPrecedence(isOperator, operator, typeAtom())
  }

  private def typeAtom(): Result[TypeExprNode] = peek match {
    case _: TkSymbol => symbolType()
    case _: TkParenLeft => tupleType()
    case _: TkBracketLeft => listType()
    case _: TkShapeStart => shapeType()
    case _ => namedOrInstantiatedType()
  }

  private def namedOrInstantiatedType(): Result[TypeExprNode] = {
    val typeName = typeNamePath().map(TypeNameNode).getOrElse {
      // TODO (syntax): Report error: Type name expected.
      return Failure
    }

    if (peekIs[TkBracketLeft]) {
      val (typeArgs, typeArgsPosition) = bracketList(typeExpression()).getOrElse(return Failure)
      val position = Position(fragment, typeName.position.startIndex, typeArgsPosition.endIndex)
      InstantiatedTypeNode(typeName, typeArgs, position).success
    } else typeName.success
  }

  private def symbolType(): Result[SymbolTypeNode] =
    consume[TkSymbol].map {
      token => SymbolTypeNode(token.name, token.position)
    }

  /**
    * The parser for tuple types doesn't parse enclosed types with a syntax `(A)` as tuples, because this would clash
    * with the concept of an enclosed type. However, we still want to be able to parse function types that work on
    * single tuple arguments. The syntax `(Int, Int) => Int` would create a function type with two arguments, so we need
    * a special syntax. The solution is to parse `((Int, Int))` as a *nested* tuple.
    */
  private def tupleType(): Result[TypeExprNode] =
    parenList(typeExpression()).map {
      case (Vector(elementType), _) if !elementType.isInstanceOf[TupleTypeNode] =>
        // An enclosed type. The parentheses are not to create a tuple, but for syntactic convenience.
        elementType

      case (elementTypes, position) => TupleTypeNode(elementTypes, position)
    }

  private def listType(): Result[ListTypeNode] = bracketEnclosure(typeExpression()).map(ListTypeNode.tupled)

  private def shapeType(): Result[ShapeTypeNode] =
    collectSepEnclosedWithOptionalIndentation[
      ShapeTypePropertyNode,
      TkShapeStart,
      TkBraceRight,
    ](consumeIf[TkComma]) { shapeTypeProperty() }.map(ShapeTypeNode.tupled)

  private def shapeTypeProperty(): Result[ShapeTypePropertyNode] = {
    val propertyName = name().getOrElse {
      // TODO (syntax): Report error: Shape type property name expected.
      return Failure
    }
    val propertyType = typing().getOrElse(return Failure)
    val position = propertyName.position.to(propertyType.position)
    ShapeTypePropertyNode(propertyName, propertyType, position).success
  }
}
