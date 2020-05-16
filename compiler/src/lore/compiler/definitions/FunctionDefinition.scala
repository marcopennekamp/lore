package lore.compiler.definitions

import lore.compiler.ast.ExprNode
import lore.compiler.feedback.Position
import lore.types.Type

class FunctionDefinition(
  val name: String, val parameters: List[ParameterDefinition], outputType: Type, override val body: Option[ExprNode],
  override val position: Position,
) extends CallTarget {
  val isAbstract: Boolean = body.isEmpty
  override lazy val signature: FunctionSignature = FunctionSignature(name, parameters, outputType)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
  override lazy val hashCode: Int = signature.hashCode
}
