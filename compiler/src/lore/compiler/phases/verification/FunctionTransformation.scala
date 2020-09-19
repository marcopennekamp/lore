package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature}
import lore.compiler.syntax.visitor.StmtVisitor

/**
  * For a given function, builds a semantic expression tree from the body's abstract syntax tree. It infers and checks
  * expression types and checks all other constraints on expressions of that function's body.
  */
object FunctionTransformation {

  /**
    * Builds a semantic expression tree from the function body's abstract syntax tree. Infers and checks types of the
    * given function body and applies function transformations. Ensures that all other expression constraints hold.
    * Also ensures that the return type of the signature is sound compared to the type of the body.
    */
  def transform(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    SignatureConstraints.verify(function.signature).flatMap { _ =>
      transformBody(function).flatMap { body =>
        body.map(body => verifyOutputType(function.signature, body)).getOrElse(Verification.succeed).map { _ =>
          function.body = body
        }
      }
    }
  }

  private def transformBody(function: FunctionDefinition)(implicit registry: Registry): Compilation[Option[Expression]] = {
    // TODO: A Unit function should manually add a return value of () if the last expression's value isn't already that.
    //       Otherwise the function won't compile, because the last expression doesn't fit the expected return type.
    //          action foo() { concat([12], [15]) }  <-- doesn't compile (concat returns a list)
    function.bodyNode.map { bodyNode =>
      ReturnConstraints.verify(bodyNode).flatMap { _ =>
        val visitor = new FunctionTransformationVisitor(function.signature, function.typeScope)
        StmtVisitor.visit(visitor)(bodyNode)
      }
    }.toCompiledOption
  }

  case class IllegallyTypedBody(signature: FunctionSignature, body: Expression) extends Error(signature.position) {
    override def message: String = s"The function ${signature.name} should return a value of type ${signature.outputType}, but actually returns" +
      s" a value of type ${body.tpe}."
  }

  /**
    * Verifies that the function's output type is compatible with the type of the body. If the body type is not
    * compatible, it might be the case that all paths of the body's last expression return a valid value. In such
    * a case, the function is valid as well, because we can guarantee at compile-time that the right kind of value
    * is returned before the end of the body is reached. This special case is also handled by this verification.
    */
  private def verifyOutputType(signature: FunctionSignature, body: Expression): Verification = {
    if (body.tpe <= signature.outputType || allPathsReturn(body)) {
      Verification.succeed
    } else {
      Verification.fromErrors(Vector(IllegallyTypedBody(signature, body)))
    }
  }

  /**
    * Whether all paths that could be taken during the evaluation of the expression definitely end in a return.
    * If that is the case, and such an expression is the last expression in a function, we can be sure that the
    * function returns the correct value regardless of the type of the actual expression.
    *
    * We only look at the last expression of a function block to decide whether the returns suffice. That is only
    * valid because we combine it with dead code analysis, dead code resulting in an error. A function like the
    * following thus could never be valid:
    *   function foo(): Int = {
    *     return 5
    *     'You fool!'
    *   }
    */
  private def allPathsReturn(expression: Expression): Boolean = {
    expression match {
      case Expression.Return(_, _) => true
      case Expression.Block(expressions, _) => expressions.lastOption.exists(allPathsReturn)
      case Expression.IfElse(_, onTrue, onFalse, _, _) => allPathsReturn(onTrue) && allPathsReturn(onFalse)

      // Loops aren't guaranteed to run even once and so cannot guarantee that all paths end in a return.
      // Hence Expression.WhileLoop and Expression.ForLoop will also result in false.
      case _ => false
    }
  }

}
