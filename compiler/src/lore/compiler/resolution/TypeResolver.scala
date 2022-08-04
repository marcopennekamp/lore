package lore.compiler.resolution

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.resolution.Resolver.withRegistryScopes
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TermScope, TypeScope}
import lore.compiler.syntax.{DeclNode, TypeExprNode}
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionTuple2Extension, OptionVectorExtension}

object TypeResolver {

  /**
    * @param termScope The term scope is required to properly resolve modules containing types, for qualified type
    *                  names.
    */
  def resolve(expression: TypeExprNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): Option[Type] = {
    expression match {
      case node@TypeExprNode.TypeNameNode(_, position) => typeScope.resolveStatic(node.namePath, position).map {
        case tpe: NamedType => tpe
        case schema: NamedSchema => schema.instantiate(Vector.empty, expression.position)
      }

      case TypeExprNode.InstantiationNode(nameNode, argumentNodes, _) => typeScope.resolveStatic(nameNode.namePath, nameNode.position).map {
        case tpe: NamedType =>
          reporter.error(TypingFeedback.Schema.ConstantUseRequired(tpe, expression))
          tpe
        case schema: NamedSchema =>
          schema.instantiate(argumentNodes.map(resolve), expression.position)
      }

      case TypeExprNode.SymbolNode(name, _) => Some(SymbolType(name))
      case TypeExprNode.SumNode(expressions, _) => expressions.map(resolve).sequence.map(SumType.construct)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(resolve).sequence.map(IntersectionType.construct)
      case TypeExprNode.TupleNode(expressions, _) => expressions.map(resolve).sequence.map(TupleType(_))
      case TypeExprNode.UnitNode(_) => Some(TupleType.UnitType)
      case TypeExprNode.FunctionNode(input, output, _) => (resolve(input).map(Type.tupled), resolve(output)).sequence.map(FunctionType.tupled)
      case TypeExprNode.ListNode(element, _) => resolve(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (resolve(key), resolve(value)).sequence.map(MapType.tupled)

      case node@TypeExprNode.ShapeNode(properties, _) =>
        Some(
          ShapeType(
            properties
              .filterDuplicates(_.name, TypingFeedback.Shape.DuplicateProperty)
              .map(expression => ShapeType.Property(expression.name, resolve(expression.tpe).getOrElse(BasicType.Any)))
          )
        )
    }
  }

  def withTypeParameters[R](
    localModule: LocalModule,
    typeParameterNodes: Vector[DeclNode.TypeVariableNode],
    resolveBounds: Boolean = true,
  )(
    f: TypeScope => TermScope => Vector[TypeVariable] => R,
  )(implicit registry: Registry, reporter: Reporter): R = {
    withRegistryScopes(localModule) {
      typeScope =>
        implicit termScope =>
          val typeParameters = TypeVariableResolver.resolve(typeParameterNodes, typeScope, resolveBounds)
          val typeParameterScope: TypeScope = ImmutableTypeScope.from(typeParameters, typeScope)
          f(typeParameterScope)(termScope)(typeParameters)
    }
  }

}
