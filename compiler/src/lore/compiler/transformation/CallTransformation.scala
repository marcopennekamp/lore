package lore.compiler.transformation

import lore.compiler.feedback.{ExpressionFeedback, Reporter, StructFeedback}
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.resolution.TypeResolver
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedBindingAccess, UntypedConstructorCall, UntypedHole, UntypedIntrinsicCall, UntypedMultiFunctionCall, UntypedValueCall}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.MultiReference
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.ExprNode.{IntrinsicCallNode, SimpleCallNode}
import lore.compiler.types.BasicType

object CallTransformation {

  def transformSimpleCall(
    node: SimpleCallNode,
    arguments: Vector[UntypedExpression],
  )(implicit termScope: TermScope, reporter: Reporter): UntypedExpression = {
    lazy val hole = UntypedHole(BasicType.Nothing, node.position)

    AccessTransformation.transform(node.namePathNode).map {
      case UntypedBindingAccess(mf: MultiFunctionDefinition, _) =>
        UntypedMultiFunctionCall(MultiReference.single(mf), arguments, node.position)

      case UntypedBindingAccess(AmbiguousMultiFunction(multiReference), _) =>
        UntypedMultiFunctionCall(multiReference, arguments, node.position)

      case UntypedBindingAccess(binding: StructConstructorBinding, _) =>
        UntypedConstructorCall(binding, arguments, node.position)

      case UntypedBindingAccess(structObject: StructObjectBinding, _) =>
        reporter.error(StructFeedback.Object.NoConstructor(structObject.name, node.position))
        hole

      case access => UntypedValueCall(access, arguments, node.position)
    }.getOrElse(hole)
  }

  def transformIntrinsicCall(
    node: IntrinsicCallNode,
    arguments: Vector[UntypedExpression],
  )(implicit typeScope: TypeScope, termScope: TermScope, reporter: Reporter): UntypedExpression = {
    val name = node.nameLiteral.value
    val resultType = TypeResolver.resolve(node.resultType).getOrElse(BasicType.Nothing)

    // We have to check the existence and arity of the invoked intrinsic before we can create the call expression.
    PoemIntrinsic.intrinsicsMap.get(name) match {
      case Some(intrinsic) =>
        if (intrinsic.arity != arguments.length) {
          reporter.error(ExpressionFeedback.Intrinsic.IllegalArity(node, intrinsic, arguments.length))
        }
        UntypedIntrinsicCall(intrinsic, arguments, resultType, node.position)

      case None =>
        reporter.error(ExpressionFeedback.Intrinsic.NotFound(node, name))
        UntypedHole(resultType, node.position)
    }
  }

}
