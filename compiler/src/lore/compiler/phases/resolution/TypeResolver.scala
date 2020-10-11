package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension

import scala.reflect.ClassTag

/**
  * Resolves types from their respective type declaration nodes.
  */
object TypeResolver {
  case class IllegalImplements(node: TypeDeclNode.StructNode) extends Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait but some other type."
  }

  /**
    * Resolves a struct type. This not only resolves the traits that the struct implements, but also the component
    * types that are implicitly part of the struct's supertypes.
    */
  def resolve(node: TypeDeclNode.StructNode)(implicit registry: Registry): Compilation[StructType] = {
    implicit val typeScope: TypeScope = registry.typeScope
    implicit val position: Position = node.position
    (
      node.implemented.map(resolveType[TraitType](IllegalImplements(node))).simultaneous,
      node.members.filterType[TypeDeclNode.ComponentNode].map(resolveComponentType).simultaneous,
    ).simultaneous.map { case (implementedTraits, componentTypes) =>
      val supertypes = implementedTraits ++ componentTypes.map(ComponentType)
      new StructType(node.name, supertypes, !node.isIndependent)
    }
  }

  case class IllegalExtends(node: TypeDeclNode.TraitNode) extends Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait or component but some other type."
  }

  def resolve(node: TypeDeclNode.TraitNode)(implicit registry: Registry): Compilation[TraitType] = {
    implicit val position: Position = node.position
    (
      node.extended.map(resolveType[TraitType](IllegalExtends(node))).simultaneous,
      node.components.map(resolveType[DeclaredType](IllegalExtends(node))).simultaneous,
    ).simultaneous.map { case (extendedTraits, componentTypes) =>
      val supertypes = extendedTraits ++ componentTypes.map(ComponentType)
      new TraitType(node.name, supertypes, !node.isIndependent)
    }
  }

  private def resolveType[T <: Type](error: => Error)(name: String)(
    implicit tag: ClassTag[T], registry: Registry, position: Position
  ): Compilation[T] = {
    registry.resolveType(name).flatMap {
      case supertype: T => supertype.compiled
      case _ => Compilation.fail(error)
    }
  }

  case class ComponentMustBeDeclaredType(node: TypeDeclNode.ComponentNode) extends Error(node) {
    override def message = s"The component ${node.name} is not a valid struct or trait."
  }

  case class ComponentMustBeOwnable(node: TypeDeclNode.ComponentNode) extends Error(node) {
    override def message = s"The component ${node.name} cannot be declared because the underlying declared type is independent."
  }

  def resolveComponentType(node: TypeDeclNode.ComponentNode)(implicit typeScope: TypeScope): Compilation[DeclaredType] = {
    typeScope.resolve(node.name)(node.position)
      .require(_.isInstanceOf[DeclaredType])(ComponentMustBeDeclaredType(node))
      .map(_.asInstanceOf[DeclaredType])
      .require(_.isOwnable)(ComponentMustBeOwnable(node))
  }
}
