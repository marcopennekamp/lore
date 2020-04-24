package lore.definitions

import lore.ast.ExprNode
import lore.compiler.feedback.Position
import lore.types.{ProductType, Subtyping, Type}

case class FunctionDefinition(
  name: String, parameters: List[ParameterDefinition], outputType: Type, body: Option[ExprNode],
  override val position: Position,
) extends PositionedDefinition {
  val isAbstract: Boolean = body.isEmpty
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  lazy val signature: FunctionSignature = FunctionSignature(name, inputType, outputType)
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
}

object FunctionDefinition {
  implicit class FunctionSetExtension(functions: Set[FunctionDefinition]) {
    // TODO: Rather than this, which will surely lead to performance problems, build a subtyping tree
    //       that can be traversed rather quickly. That would also be useful for the Javascript runtime,
    //       especially if we manage to supplement the runtime with Lore compiler classes using ScalaJS.
    def multiMin: Set[FunctionDefinition] = {
      functions.filter(f => !functions.filter(f2 => f2 != f).exists(f2 => Subtyping.isSubtype(f2.inputType, f.inputType)))
    }
  }
}