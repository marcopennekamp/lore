package lore.compiler.transformation

import lore.compiler.feedback.{ExpressionFeedback, Reporter}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.ModuleDefinition
import lore.compiler.semantics.scopes.{Binding, BindingScope}
import lore.compiler.syntax.Node.{NameNode, NamePathNode}

object AccessTransformation {

  /**
    * Transforms a name path node, whose underlying name path either refers to a module member or represents a member
    * access chain, into the correct kind of expression.
    *
    * If the path refers to a module member or a local non-module binding, the binding is processed with
    * `processSingle`. If the path instead refers to a non-module binding <i>and</i> the path has multiple segments,
    * this initial binding is processed with `processInstance`, before additional member accesses are built.
    */
  def transform(
    processSingle: Binding => Option[Expression],
    processInstance: Binding => Option[Expression],
  )(namePathNode: NamePathNode)(implicit bindingScope: BindingScope, judgmentCollector: JudgmentCollector, reporter: Reporter): Option[Expression] = {
    val namePath = namePathNode.namePath
    val position = namePathNode.position

    bindingScope.resolve(namePath.headName, position).flatMap {
      case module: ModuleDefinition =>
        if (namePath.isMultiple) {
          bindingScope.resolveGlobal(module.name ++ namePath.tail, position).flatMap(processSingle)
        } else {
          reporter.error(ExpressionFeedback.IllegalModuleValue(module, position))
          None
        }
      case binding if namePath.isSingle => processSingle(binding)
      case binding => processInstance(binding).map(transformMemberAccess(_, namePathNode.segments.tail))
    }
  }

  def transform(
    process: Binding => Option[Expression],
  )(namePathNode: NamePathNode)(implicit bindingScope: BindingScope, judgmentCollector: JudgmentCollector, reporter: Reporter): Option[Expression] = {
    transform(process, process)(namePathNode)
  }

  /**
    * Transforms a member access chain on `instance` into a sequence of member access expressions. The member cannot be
    * resolved until the instance's type has been inferred. Hence this function returns an "unresolved member access"
    * expression, which will be resolved later.
    *
    * Note that `instance` is returned unchanged if `memberNames` is empty.
    */
  def transformMemberAccess(instance: Expression, memberNames: Vector[NameNode])(implicit judgmentCollector: JudgmentCollector): Expression = {
    memberNames.foldLeft(instance) {
      case (expression, NameNode(memberName, position)) =>
        val memberType = new InferenceVariable
        judgmentCollector.add(TypingJudgment.MemberAccess(memberType, instance.tpe, memberName, position))
        Expression.UnresolvedMemberAccess(expression, memberName, memberType, position)
    }
  }

}
