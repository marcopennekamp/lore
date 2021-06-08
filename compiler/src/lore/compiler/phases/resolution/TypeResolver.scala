package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.{TypeDeclNode, TypeExprNode}
import lore.compiler.types._

/**
  * Resolves types from their respective type declaration nodes.
  */
object TypeResolver {

  case class IllegalImplements(node: TypeDeclNode.StructNode) extends Feedback.Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.AliasNode)(implicit registry: Registry): Compilation[Type] = {
    implicit val typeScope: TypeScope = registry.typeScope
    TypeExpressionEvaluator.evaluate(node.tpe)
  }

  def resolve(node: TypeDeclNode.StructNode)(implicit registry: Registry): Compilation[StructType] = {
    implicit val typeScope: TypeScope = registry.typeScope
    implicit val position: Position = node.position

    node.implemented.map(resolveInheritedTypes(IllegalImplements(node))).simultaneous.map(_.flatten).map { supertypes =>
      new StructType(node.name, supertypes)
    }
  }

  case class IllegalExtends(node: TypeDeclNode.TraitNode) extends Feedback.Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait or shape but some other type."
  }

  def resolve(node: TypeDeclNode.TraitNode)(implicit registry: Registry): Compilation[TraitType] = {
    implicit val position: Position = node.position

    node.extended.map(resolveInheritedTypes(IllegalExtends(node))).simultaneous.map(_.flatten).map { supertypes =>
      new TraitType(node.name, supertypes)
    }
  }

  /**
    * Evaluates and extracts all inherited types from the given AST type as such:
    *   - Traits and shapes can be inherited from directly.
    *   - It is also possible to inherit from intersection types, but only if the intersection only contains traits
    *     and/or shapes itself. The rationale for supporting this is simple: If we created a type alias that combines
    *     traits and shapes, for example to support a specific aspect of component-based programming, we want the
    *     language user to be able to use that type for inheritance.
    *   - All other types cannot be inherited from and result in an error.
    */
  private def resolveInheritedTypes(error: => Feedback.Error)(expr: TypeExprNode)(implicit registry: Registry, position: Position): Compilation[Vector[Type]] = {
    def extract(tpe: Type): Compilation[Vector[Type]] = tpe match {
      case supertrait: TraitType => Vector(supertrait).compiled
      case shape: ShapeType => Vector(shape).compiled
      case IntersectionType(parts) => parts.toVector.map(extract).simultaneous.map(_.flatten)
      case _ => Compilation.fail(error)
    }

    TypeExpressionEvaluator.evaluate(expr)(registry.typeScope).flatMap(extract)
  }

}
