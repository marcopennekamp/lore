package lore.definitions
import lore.ast.ExprNode
import lore.compiler.Position
import lore.types.{ProductType, Type}

case class FunctionDefinition(
  name: String, parameters: List[ParameterDefinition], outputType: Type, body: Option[ExprNode],
  override val position: Position,
) extends PositionedDefinition {
  val isAbstract: Boolean = body.isEmpty
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  lazy val signature: FunctionSignature = FunctionSignature(name, inputType, outputType)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
}
