package lore.compiler.phases.resolution

import lore.compiler.ast.ExprNode
import lore.compiler.ast.ExprNode.{ConcatenationNode, SimpleCallNode, StringLiteralNode}
import lore.compiler.ast.transformer.StmtTransformer
import lore.compiler.core.Compilation

/**
  * Surrounds any non-literal expression in a concatenation with a toString function, if one is available. The
  * point of this, as opposed to a text transformation during transpilation, is that we will be able to employ
  * the usual compiler paths for resolving functions. It's much better than hard-coding a toString JS function
  * call.
  */
class ConcatToStringTransformer extends StmtTransformer {
  // TODO: We actually have to ensure that the current context has a registered function "toString".
  // TODO: Do we HAVE to hard-code the function name? Maybe this could be specified as a compiler option? Or
  //       am I overthinking this. The idea was, from the start, to make pyramid an "optional" drop-in. So this
  //       feels like one step of coupling the the compiler and pyramid together. We'll have to see.
  // TODO: Remember to adjust the name once we have introduced modules et cetera.

  override def transform(node: ConcatenationNode)(expressions: List[ExprNode]): Compilation[ExprNode] = {
    val transformedExpressions = expressions.map {
      case literal: StringLiteralNode => literal
      case expr => SimpleCallNode("toString", None, List(expr))
    }
    Compilation.succeed(ConcatenationNode(transformedExpressions))
  }
}
