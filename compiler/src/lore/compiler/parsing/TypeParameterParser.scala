package lore.compiler.parsing

import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.syntax.Node.{NameNode, withPosition}
import lore.compiler.syntax.{DeclNode, TypeExprNode}
import lore.compiler.types.TypeVariable.Variance

/**
  * @param whitespace This can be manually specified to disable newlines in whitespace.
  */
class TypeParameterParser(nameParser: NameParser)(implicit fragment: Fragment, whitespace: P[Any] => P[Unit]) {
  private val typeParser = new TypeParser(nameParser)

  import nameParser._
  import typeParser.typeExpression

  def simpleParameter[_: P]: P[DeclNode.TypeVariableNode] = {
    P(Index ~~ typeVariableCommons ~~ Index)
      .map { case (index1, (name, lowerBound, upperBound), index2) => (index1, name, lowerBound, upperBound, index2) }
      .map(withPosition(DeclNode.TypeVariableNode.simple _))
  }

  def traitParameter[_: P]: P[DeclNode.TypeVariableNode] = {
    P(Index ~~ variance ~ typeVariableCommons ~~ Index)
      .map { case (index1, variance, (name, lowerBound, upperBound), index2) => (index1, name, lowerBound, upperBound, variance, index2) }
      .map(withPosition(DeclNode.TypeVariableNode.variant _))
  }

  def structParameter[_: P]: P[DeclNode.TypeVariableNode] = {
    // We could require covariance ("+") for open parameters here, but parser error messages aren't very useful. Hence,
    // it's better to explicitly check this in a later phase.
    P(Index ~~ "open".!.?.map(_.isDefined) ~ variance ~ typeVariableCommons ~~ Index)
      .map { case (index1, isOpen, variance, (name, lowerBound, upperBound), index2) => (index1, name, lowerBound, upperBound, variance, isOpen, index2) }
      .map(withPosition(DeclNode.TypeVariableNode.apply _))
  }

  private def typeVariableCommons[_: P]: P[(NameNode, Option[TypeExprNode], Option[TypeExprNode])] = {
    P(typeVariableName ~ (">:" ~ typeExpression).? ~ ("<:" ~ typeExpression).?)
  }

  private def variance[_: P]: P[Variance] = P(("+".! | "-".!).?).map {
    case Some("+") => Variance.Covariant
    case Some("-") => Variance.Contravariant
    case None => Variance.Invariant
  }
}
