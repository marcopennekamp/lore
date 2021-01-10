package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

import scala.reflect.ClassTag

/**
  * Resolves types from their respective type declaration nodes.
  */
object TypeResolver {

  case class IllegalImplements(node: TypeDeclNode.StructNode) extends Error(node) {
    override def message = s"The struct ${node.name} does not implement a trait but some other type."
  }

  def resolve(node: TypeDeclNode.AliasNode)(implicit registry: Registry): Compilation[Type] = {
    implicit val typeScope: TypeScope = registry.typeScope
    TypeExpressionEvaluator.evaluate(node.tpe)
  }

  def resolve(node: TypeDeclNode.StructNode)(implicit registry: Registry): Compilation[StructType] = {
    implicit val typeScope: TypeScope = registry.typeScope
    implicit val position: Position = node.position

    node.implemented.map(resolveType[TraitType](IllegalImplements(node))).simultaneous.map { supertypes =>
      new StructType(node.name, supertypes)
    }
  }

  case class IllegalExtends(node: TypeDeclNode.TraitNode) extends Error(node) {
    override def message = s"The trait ${node.name} does not extend a trait but some other type."
  }

  def resolve(node: TypeDeclNode.TraitNode)(implicit registry: Registry): Compilation[TraitType] = {
    implicit val position: Position = node.position

    node.extended.map(resolveType[TraitType](IllegalExtends(node))).simultaneous.map { supertypes =>
      new TraitType(node.name, supertypes)
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

}
