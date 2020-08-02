package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.Registry
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types._

/**
  * Resolves types from their respective type declaration nodes.
  */
object TypeResolver {
  case class LabelMustExtendLabel(node: TypeDeclNode.LabelNode) extends Error(node) {
    override def message = s"The label ${node.name} does not extend a label but some other type."
  }

  def resolve(node: TypeDeclNode.LabelNode)(implicit registry: Registry): Compilation[LabelType] = {
    node.supertypeName match {
      case None => new LabelType(node.name, None).compiled
      case Some(supertypeName) =>
        registry.resolveType(supertypeName)(node.position).flatMap {
          case supertype: LabelType => new LabelType(node.name, Some(supertype)).compiled
          case _ => Compilation.fail(LabelMustExtendLabel(node))
        }
    }
  }

  case class ClassMustExtendClass(node: TypeDeclNode.ClassNode) extends Error(node) {
    override def message = s"The class ${node.name} does not extend a class but some other type."
  }

  def resolve(node: TypeDeclNode.ClassNode)(implicit registry: Registry): Compilation[ClassType] = {
    node.supertypeName match {
      case None => new ClassType(node.name, None, node.isAbstract).compiled
      case Some(supertypeName) =>
        registry.resolveType(supertypeName)(node.position).flatMap {
          case supertype: ClassType => new ClassType(node.name, Some(supertype), node.isAbstract).compiled
          case _ => Compilation.fail(ClassMustExtendClass(node))
        }
    }
  }
}
