package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{ExpressionFeedback, Reporter}
import lore.compiler.semantics.bindings.{StructBinding, StructConstructorBinding, StructObjectBinding, TermBinding}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedAccess, UntypedBindingAccess, UntypedMemberAccess}
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.Node.{NameNode, NamePathNode}

object AccessTransformation {

  /**
    * Transforms a name path node, whose underlying name path either refers to a module member or represents a member
    * access chain, into the correct kind of expression.
    *
    * If the full path refers to a module member or a local binding, without any member accesses, the result will be an
    * [[UntypedBindingAccess]]. Otherwise, the result expression will be an [[UntypedMemberAccess]].
    */
  def transform(namePathNode: NamePathNode)(
    implicit termScope: TermScope,
    reporter: Reporter,
  ): Option[UntypedAccess] = {
    val headNameNode = namePathNode.segments.head
    termScope.resolve(headNameNode.value, headNameNode.position).flatMap { initialBinding =>
      resolveAccessInstance(
        initialBinding,
        namePathNode.segments.tail,
        headNameNode.position,
      ).map { case (instance, memberNames) =>
        // The instance is an UntypedBindingAccess and `transformMemberAccess` produces at most an UntypedMemberAccess,
        // so the cast here is legal.
        transformMemberAccess(instance, memberNames).asInstanceOf[UntypedAccess]
      }
    }
  }

  /**
    * Resolves the instance on which the member access can be performed given an initial binding and the remaining name
    * nodes. If the resulting name node vector is empty, the resulting binding is understood as a single value. The
    * result position covers the whole result binding, from the first to the last name node.
    *
    * The point of this function is to find the place at which module resolution stops and actual property access
    * starts.
    */
  private def resolveAccessInstance(
    binding: TermBinding,
    remaining: Vector[NameNode],
    bindingPosition: Position,
  )(implicit termScope: TermScope, reporter: Reporter): Option[(UntypedBindingAccess, Vector[NameNode])] = {
    resolveAccessInstanceBinding(
      binding,
      remaining,
      bindingPosition,
      bindingPosition,
    ).map { case (instanceBinding, fullPosition, remaining) =>
      (UntypedBindingAccess(instanceBinding, fullPosition), remaining)
    }
  }

  private def resolveAccessInstanceBinding(
    binding: TermBinding,
    remaining: Vector[NameNode],
    bindingPosition: Position,
    fullPosition: Position,
  )(implicit termScope: TermScope, reporter: Reporter): Option[(TermBinding, Position, Vector[NameNode])] = {
    if (remaining.isEmpty) {
      return binding match {
        case module: GlobalModule =>
          reporter.error(ExpressionFeedback.IllegalModuleValue(module, bindingPosition))
          None
        case binding => Some((binding, fullPosition, Vector.empty))
      }
    }

    val nameNode = remaining.head
    lazy val simpleResult = Some((binding, fullPosition, remaining))

    def rec(binding: TermBinding) = {
      resolveAccessInstanceBinding(binding, remaining.tail, nameNode.position, fullPosition.to(nameNode))
    }

    def handleCompanionModule(binding: StructBinding) = {
      // If the companion module doesn't exist or if the member cannot be found in the companion module, the member
      // access must be a UCS call which will be resolved during typing.
      binding.companionModule.flatMap(module => termScope.global(module.name + nameNode.value)) match {
        case Some(member) => rec(member)
        case None => simpleResult
      }
    }

    binding match {
      case module: GlobalModule =>
        termScope
          .resolveGlobal(module.name + nameNode.value, nameNode.position)
          .flatMap(rec)

      case binding: StructConstructorBinding => handleCompanionModule(binding)

      case binding: StructObjectBinding =>
        // If the struct object contains a property with the given name, we can be sure (by constraint) that the
        // companion module doesn't contain a member with such a name and thus the struct object binding is the actual
        // instance. In all other cases, the struct object either must have a companion module with the requisite
        // member, or the member name is treated as a UCS call during typing.
        if (binding.tpe.member(nameNode.value).isDefined) {
          Some((binding, fullPosition, remaining))
        } else {
          handleCompanionModule(binding)
        }

      case _ => simpleResult
    }
  }

  /**
    * Transforms a member access chain on `instance` into a sequence of member access expressions. Note that `instance`
    * is returned unchanged if `memberNames` is empty.
    */
  def transformMemberAccess(
    instance: UntypedExpression,
    memberNames: Vector[NameNode],
  )(implicit termScope: TermScope): UntypedExpression = {
    memberNames.foldLeft(instance) {
      case (expression, NameNode(memberName, position)) =>
        // The position from `expression.position` to `position` covers the whole access chain. For example, accessing
        // a member `length` of an expression `company.employees` will span all of `company.employees.length`.
        UntypedMemberAccess(expression, memberName, termScope.get(memberName), expression.position.to(position))
    }
  }

}
