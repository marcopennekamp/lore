package lore.compiler

import lore.ast.TypeDeclNode
import lore.definitions._
import lore.types.{ClassType, LabelType, Type}

object TypeDeclarationResolver {
  /**
    * Resolves a given type declaration. Alias types are resolved merely to a type object, while labels and classes
    * are resolved to [[DeclaredTypeDefinition]].
    */
  def resolve(node: TypeDeclNode)(implicit registry: Registry): C[Either[Type, DeclaredTypeDefinition]] = {
    node match {
      case TypeDeclNode.AliasNode(_, tpe) => TypeExpressionEvaluator.evaluate(tpe).map(Left(_))
      case labelNode: TypeDeclNode.LabelNode => resolveLabelNode(labelNode).map(Right(_))
      case classNode: TypeDeclNode.ClassNode => resolveClassNode(classNode).map(Right(_))
    }
  }

  private def resolveLabelNode(node: TypeDeclNode.LabelNode)(implicit registry: Registry): C[LabelDefinition] = {
    for {
      supertype <- node.supertypeName.map(name => registry.resolveType(name, node)).toCompiledOption.require { option =>
        // Ensure that, if the label type extends another type, that type is also a label type.
        option.forall(_.isInstanceOf[LabelType])
      }(Error.LabelMustExtendLabel(node))
    } yield {
      val tpe = new LabelType(supertype.asInstanceOf[Option[LabelType]])
      val definition = new LabelDefinition(node.name, tpe)
      tpe.initialize(definition)
      definition
    }
  }

  private def resolveClassNode(node: TypeDeclNode.ClassNode)(implicit registry: Registry): C[ClassDefinition] = {
    // TODO: This should be a simultaneous compilation rather than a sequential one, as we can resolve the
    //       owned-by type, members, and constructors in parallel. This would allow us to display member errors
    //       even when there is also an error with the owned-by declaration.
    for {
      supertype <- node.supertypeName.map(name => registry.resolveType(name, node)).toCompiledOption.require { option =>
        // Ensure that, if the class type extends another type, that type is also a class type.
        option.forall(_.isInstanceOf[ClassType])
      }(Error.ClassMustExtendClass(node))
      ownedBy <- node.ownedBy.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      members <- node.members.map(resolveMemberNode).combine
      constructors <- node.constructors.map(FunctionDeclarationResolver.resolveConstructorNode).combine
    } yield {
      val tpe = new ClassType(supertype.asInstanceOf[Option[ClassType]], ownedBy, node.isAbstract)
      val definition = new ClassDefinition(node.name, tpe, members, constructors)
      tpe.initialize(definition)
      definition
    }
  }

  private def resolveMemberNode(node: TypeDeclNode.MemberNode)(implicit registry: Registry): C[MemberDefinition[Type]] = {
    node match {
      case TypeDeclNode.PropertyNode(name, tpe, isMutable) =>
        val resolveType = () => TypeExpressionEvaluator.evaluate(tpe)
        Compilation.succeed(new PropertyDefinition(name, resolveType, isMutable))
      case componentNode@TypeDeclNode.ComponentNode(name, overrides) =>
        val resolveType = () => {
          registry.resolveType(name, node)
            .require(_.isInstanceOf[ClassType])(Error.ComponentMustBeClass(componentNode))
            .map(_.asInstanceOf[ClassType])
        }
        Compilation.succeed(new ComponentDefinition(name, resolveType, overrides))
    }
  }
}
