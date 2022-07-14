package lore.compiler.resolution

import lore.compiler.feedback.{Reporter, SchemaFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionExtension

object DeclaredSchemaResolver {

  /**
    * Initializes `schema`. (See the guidelines in [[lore.compiler.semantics.definitions.BindingDefinition]].)
    */
  def initialize(schema: DeclaredSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    Resolver.withTypeParameters(schema.localModule, schema.node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val supertypes = resolveInheritedTypes(schema, schema.node.extended)
        schema.initialize(typeParameters, supertypes)
    }
  }

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
  private def resolveInheritedTypes(schema: DeclaredSchema, nodes: Vector[TypeExprNode])(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): Vector[Type] = {
    def extract(tpe: Type): Vector[Type] = tpe match {
      case supertrait: TraitType => Vector(supertrait)
      case shape: ShapeType => Vector(shape)
      case IntersectionType(parts) => parts.toVector.flatMap(extract)
      case _ =>
        val supertypeName = Some(tpe).filterType[NamedType].map(_.name)
        reporter.error(SchemaFeedback.IllegalExtends(schema, supertypeName))
        Vector.empty
    }

    nodes
      .flatMap(TypeExpressionEvaluator.evaluate)
      .flatMap(extract)
      .distinct
  }

  /**
    * Fallback-initializes `schema`, initializing type parameters without bounds and ignoring inherited types. This
    * happens before any other schemas are initialized, so the schema may not reference any other declared types.
    */
  def fallbackInitialize(schema: DeclaredSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    Resolver.withTypeParameters(schema.localModule, schema.node.typeVariables, resolveBounds = false) {
      implicit typeScope => implicit termScope => typeParameters =>
        schema.initialize(typeParameters, Vector.empty)
    }
  }

}
