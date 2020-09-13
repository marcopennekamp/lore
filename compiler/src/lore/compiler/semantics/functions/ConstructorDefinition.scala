package lore.compiler.semantics.functions

import lore.compiler.core.Position
import lore.compiler.semantics.expressions.Expression
import lore.compiler.syntax.ExprNode
import lore.compiler.semantics.{TypeScope, functions}
import lore.compiler.semantics.structures._

/**
  * The definition of a class constructor.
  *
  * Constructor definition equality is always reference equality, as we create exactly one constructor definition
  * for every defined constructor.
  */
class ConstructorDefinition(
  override val name: String, val typeScope: TypeScope, val parameters: List[ParameterDefinition],
  val bodyNode: ExprNode.BlockNode, override val position: Position
) extends InternalCallTarget {
  private var classDefinition: StructDefinition = _
  def associateWith(classDefinition: StructDefinition): Unit = {
    this.classDefinition = classDefinition
  }
  override lazy val signature: FunctionSignature = functions.FunctionSignature(name, parameters, classDefinition.tpe, position)
  override def toString = s"${classDefinition.name}.$name(${parameters.mkString(", ")})"

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var body: Expression.Block = _
}
