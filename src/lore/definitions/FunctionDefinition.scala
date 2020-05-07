package lore.definitions

import lore.ast.ExprNode
import lore.compiler.feedback.Position
import lore.types.Type

class FunctionDefinition(
  val name: String, val parameters: List[ParameterDefinition], outputType: Type, val body: Option[ExprNode],
  override val position: Position,
) extends PositionedDefinition with CallTarget {
  val isAbstract: Boolean = body.isEmpty
  override lazy val signature: FunctionSignature = FunctionSignature(name, parameters, outputType)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
  override lazy val hashCode: Int = signature.hashCode
}
