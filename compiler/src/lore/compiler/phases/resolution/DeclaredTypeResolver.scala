package lore.compiler.phases.resolution

import lore.compiler.ast.TypeDeclNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, Fragment, Registry, TypeScope}
import lore.compiler.core.feedback.{Error, Position}
import lore.compiler.structures._
import lore.compiler.types._

object DeclaredTypeResolver {
  /**
    * Resolves a declared type declaration.
    */
  def resolveDeclaredNode(node: TypeDeclNode.DeclaredNode)(implicit registry: Registry): C[DeclaredTypeDefinition] = {
    node match {
      case labelNode: TypeDeclNode.LabelNode => resolveLabelNode(labelNode)
      case classNode: TypeDeclNode.ClassNode => resolveClassNode(classNode)
    }
  }

  case class LabelMustExtendLabel(node: TypeDeclNode.LabelNode) extends Error(node) {
    override def message = s"The label ${node.name} does not extend a label but some other type."
  }

  private def resolveLabelNode(node: TypeDeclNode.LabelNode)(implicit registry: Registry): C[LabelDefinition] = {
    implicit val position = node.position
    for {
      supertype <- node.supertypeName.map(name => registry.resolveType(name)).toCompiledOption.require { option =>
        // Ensure that, if the label type extends another type, that type is also a label type.
        option.forall(_.isInstanceOf[LabelType])
      }(LabelMustExtendLabel(node))
    } yield {
      val tpe = new LabelType(supertype.asInstanceOf[Option[LabelType]])
      val definition = new LabelDefinition(node.name, tpe, node.position)
      tpe.initialize(definition)
      definition
    }
  }

  case class ClassMustExtendClass(node: TypeDeclNode.ClassNode) extends Error(node) {
    override def message = s"The class ${node.name} does not extend a class but some other type."
  }

  private def resolveClassNode(node: TypeDeclNode.ClassNode)(implicit registry: Registry): C[ClassDefinition] = {
    implicit val position: Position = node.position
    implicit val typeScope: TypeScope = registry.typeScope
    // Resolve supertype and members simultaneously for same-run error reporting. Owned-by and constructor types
    // are resolved in the deferred typing verification phase.
    (
      node.supertypeName.map(name => registry.resolveType(name)).toCompiledOption.require { option =>
        // Ensure that, if the class type extends another type, that type is also a class type.
        option.forall(_.isInstanceOf[ClassType])
      }(ClassMustExtendClass(node)),
      node.members.map(resolveMemberNode).simultaneous,
    ).simultaneous.flatMap { case (supertype, members) =>
      node.constructors.map(FunctionDeclarationResolver.resolveConstructorNode).simultaneous.map { constructors =>
        val ownedBy = node.ownedBy.map(ob => new OwnedByDeferred(() => TypeExpressionEvaluator.evaluate(ob)))
        val tpe = new ClassType(supertype.asInstanceOf[Option[ClassType]], ownedBy, node.isAbstract)
        val definition = new ClassDefinition(node.name, tpe, node.isEntity, members, constructors, node.position)
        tpe.initialize(definition)
        definition
      }
    }
  }

  case class ComponentMustBeClass(node: TypeDeclNode.ComponentNode) extends Error(node) {
    override def message = s"The component ${node.name} is not a valid class type."
  }

  private def resolveMemberNode(node: TypeDeclNode.MemberNode)(implicit typeScope: TypeScope): C[MemberDefinition[Type]] = {
    node match {
      case TypeDeclNode.PropertyNode(name, tpe, isMutable, _) =>
        val resolveType = () => TypeExpressionEvaluator.evaluate(tpe)
        Compilation.succeed(new PropertyDefinition(name, resolveType, isMutable, node.position))
      case componentNode@TypeDeclNode.ComponentNode(name, overrides, _) =>
        val resolveType = () => {
          implicit val position: Position = node.position
          typeScope.resolve(name)
            .require(_.isInstanceOf[ClassType])(ComponentMustBeClass(componentNode))
            .map(_.asInstanceOf[ClassType])
        }
        Compilation.succeed(new ComponentDefinition(name, resolveType, overrides, node.position))
    }
  }
}
