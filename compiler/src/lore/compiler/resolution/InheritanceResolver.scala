package lore.compiler.resolution

import lore.compiler.feedback.{Feedback, Reporter}
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
    *
    * If a supertype expression cannot be evaluated, it is omitted from the list of inherited types and a proper error
    * is reported.
    */
  def resolveInheritedTypes(nodes: Vector[TypeExprNode], error: => Feedback.Error)(implicit typeScope: TypeScope, reporter: Reporter): Vector[Type] = {
    def extract(tpe: Type): Vector[Type] = tpe match {
      case supertrait: TraitType => Vector(supertrait)
      case shape: ShapeType => Vector(shape)
      case IntersectionType(parts) => parts.toVector.flatMap(extract)
      case _ =>
        reporter.error(error)
        Vector.empty
    }

    nodes
      .flatMap(TypeExpressionEvaluator.evaluate)
      .flatMap(extract)
      .distinct
  }

}
