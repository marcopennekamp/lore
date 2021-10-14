package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{ExpressionFeedback, Feedback, Reporter, StructFeedback}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.Node.{NameNode, NamePathNode}
import lore.compiler.typing.InferenceVariable

object AccessTransformation {

  /**
    * Transforms a name path node, whose underlying name path either refers to a module member or represents a member
    * access chain, into the correct kind of expression.
    *
    * If the full path refers to a module member or a local binding, without any actual member accesses, the binding is
    * processed with `processSingle`. Otherwise, if only a part of the name path refers to a sought binding and the
    * rest of the path is a member access chain, the binding is interpreted as an instance and processed with
    * `processInstance`, before the required member access expressions are built, and finally `processAccessed` is
    * invoked to handle the resulting full member access expression.
    */
  def transform(
    processSingle: Binding => Option[Expression],
    processInstance: Binding => Option[Expression],
    processAccessed: Expression => Option[Expression],
  )(namePathNode: NamePathNode)(implicit bindingScope: BindingScope, reporter: Reporter): Option[Expression] = {
    val headNameNode = namePathNode.segments.head
    bindingScope.resolve(headNameNode.value, headNameNode.position).flatMap { initialBinding =>
      resolveAccessInstance(initialBinding, namePathNode.segments.tail, headNameNode.position).flatMap {
        case (binding, memberNames) =>
          if (memberNames.isEmpty) processSingle(binding)
          else processInstance(binding).map(transformMemberAccess(_, memberNames)).flatMap(processAccessed)
      }
    }
  }

  /**
    * Resolves the instance on which member access can be performed given an initial binding and the remaining name
    * nodes. If the resulting name node vector is empty, the resulting binding is understood as a single value.
    *
    * The point of this function is to find the place at which module resolution stops and actual property access
    * starts.
    */
  private def resolveAccessInstance(
    binding: Binding,
    remaining: Vector[NameNode],
    position: Position,
  )(implicit bindingScope: BindingScope, reporter: Reporter): Option[(Binding, Vector[NameNode])] = {
    if (remaining.isEmpty) {
      return binding match {
        case module: GlobalModule =>
          reporter.error(ExpressionFeedback.IllegalModuleValue(module, position))
          None
        case binding => Some((binding, remaining))
      }
    }

    val nameNode = remaining.head

    def handleModule(module: GlobalModule) = {
      bindingScope
        .resolveGlobal(module.name + nameNode.value, nameNode.position)
        .flatMap(resolveAccessInstance(_, remaining.tail, nameNode.position))
    }

    def handleCompanionModule(binding: StructBinding, error: => Feedback.Error) = {
      binding.definition.companionModule match {
        case Some(module) => handleModule(module)
        case None =>
          reporter.error(error)
          None
      }
    }

    binding match {
      case module: GlobalModule => handleModule(module)
      case binding: StructConstructorBinding =>
        handleCompanionModule(binding, StructFeedback.CompanionModuleExpected(binding, nameNode.value, nameNode.position))
      case binding: StructObjectBinding =>
        // If the struct object contains a property with the given name, we can be sure that the companion module
        // doesn't contain a member with such a name and thus the struct object binding is the actual instance. In
        // all other cases, the struct object must have a companion module with the requisite member.
        if (binding.tpe.member(nameNode.value).isDefined) {
          Some((binding, remaining))
        } else {
          handleCompanionModule(binding, StructFeedback.Object.CompanionModuleExpected(binding, nameNode.value, nameNode.position))
        }
      case binding => Some((binding, remaining))
    }
  }

  def transform(
    process: Binding => Option[Expression],
  )(namePathNode: NamePathNode)(implicit bindingScope: BindingScope, reporter: Reporter): Option[Expression] = {
    transform(process, process, Some(_))(namePathNode)
  }

  /**
    * Transforms a member access chain on `instance` into a sequence of member access expressions. The member cannot be
    * resolved until the instance's type has been inferred. Hence this function returns an "unresolved member access"
    * expression, which will be resolved later.
    *
    * Note that `instance` is returned unchanged if `memberNames` is empty.
    */
  def transformMemberAccess(instance: Expression, memberNames: Vector[NameNode]): Expression = {
    memberNames.foldLeft(instance) {
      case (expression, NameNode(memberName, position)) =>
        Expression.UnresolvedMemberAccess(expression, memberName, new InferenceVariable, position)
    }
  }

}
