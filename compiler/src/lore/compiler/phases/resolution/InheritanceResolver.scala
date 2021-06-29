package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types.{IntersectionType, ShapeType, TraitType, Type}

object InheritanceResolver {

  /**
    * Evaluates the given type expression nodes, interpreting them as types that are inherited from. The result is a
    * flattened list of supertypes (containing only traits and shapes) that has been processed as such:
    *
    *   - Traits and shapes can be inherited from directly.
    *   - It is also possible to inherit from intersection types, but only if the intersection only contains traits
    *     and/or shapes itself. The rationale for supporting this is simple: If we created a type alias that combines
    *     traits and shapes, for example to support a specific aspect of component-based programming, we want the
    *     language user to be able to use that type for inheritance.
    *   - All other types cannot be inherited from and result in an error.
    */
  def resolveInheritedTypes(nodes: Vector[TypeExprNode], error: => Feedback.Error)(implicit typeScope: TypeScope): Compilation[Vector[Type]] = {
    def extract(tpe: Type): Compilation[Vector[Type]] = tpe match {
      case supertrait: TraitType => Vector(supertrait).compiled
      case shape: ShapeType => Vector(shape).compiled
      case IntersectionType(parts) => parts.toVector.map(extract).simultaneous.map(_.flatten)
      case _ => Compilation.fail(error)
    }

    for {
      types <- nodes.map(TypeExpressionEvaluator.evaluate).simultaneous
      supertypeLists <- types.map(extract).simultaneous
      supertypes = supertypeLists.flatten.distinct
    } yield supertypes
  }

}
