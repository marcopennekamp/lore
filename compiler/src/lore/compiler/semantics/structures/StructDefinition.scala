package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.structures.StructDefinition.LazyObjectPropertyVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.syntax.visitor.{CombiningTopLevelExprVisitor, TopLevelExprVisitor}
import lore.compiler.types.StructSchema
import scalaz.Id

class StructDefinition(
  override val name: String,
  override val schema: StructSchema,
  val properties: Vector[StructPropertyDefinition],
  val isObject: Boolean,
  override val position: Position,
) extends DeclaredSchemaDefinition {

  lazy val propertyMap: Map[String, StructPropertyDefinition] = properties.map(p => (p.name, p)).toMap

  lazy val openProperties: Vector[StructPropertyDefinition] = properties.filter(_.isOpen)

  /**
    * Whether the object instance must be lazily initialized. This is currently decided very conservatively: any usage
    * of a function, struct, or variable leads to lazy initialization.
    */
  lazy val isLazyObject: Boolean = isObject && {
    properties.exists { property =>
      property.defaultValueNode.exists { defaultValue =>
        TopLevelExprVisitor.visit(LazyObjectPropertyVisitor)(defaultValue)
      }
    }
  }

}

object StructDefinition {
  /**
    * This visitor decides whether a given property's default value must be initialized lazily.
    */
  private object LazyObjectPropertyVisitor extends CombiningTopLevelExprVisitor.OrVisitor {
    override protected def visit(node: TopLevelExprNode, results: Vector[Boolean]): Id.Id[Boolean] = node match {
      // A variable node may refer to an object or a struct constructor, which would require lazy initialization.
      case _: ExprNode.VariableNode => true
      case _: ExprNode.FixedFunctionNode => true
      case _: ExprNode.ConstructorNode => true
      case _: ExprNode.ObjectMapNode => true
      case _: ExprNode.CallNode => true
      case _: ExprNode.SimpleCallNode => true
      case _: ExprNode.DynamicCallNode => true
      case _ => false
    }
  }
}
