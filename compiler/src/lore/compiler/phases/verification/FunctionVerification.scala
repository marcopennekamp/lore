package lore.compiler.phases.verification

import lore.compiler.ast.StmtNode
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.core.{Fragment, Registry}
import lore.compiler.definitions.{ClassDefinition, ConstructorDefinition, InternalCallTarget}
import lore.types.Type

/**
  * For a given function or constructor, infers and checks expression types and checks all other constraints on
  * expressions of that function's body. After the function checker has been run without producing any compilation
  * errors, we can be sure that all expressions are soundly typed and adhere to all constraints (barring compiler bugs,
  * of course).
  *
  * Ascribing inferred types is a side-effect of this verifier.
  */
object FunctionVerification {
  case class IllegallyTypedExpression(expr: StmtNode, expectedTypes: List[Type])(implicit fragment: Fragment) extends Error(expr) {
    override def message = s"The expression $expr has the illegal type ${expr.inferredType}.$expected"
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types (or a subtype thereof): ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  /**
    * Infers and checks types of the given function or constructor body. Ensures that all other expression constraints
    * hold. Also ensures that the return type of the signature is sound compared to the type of the body.
    *
    * If a constructor is passed, classDefinition must be some value.
    */
  def verifyFunction(
    target: InternalCallTarget, classDefinition: Option[ClassDefinition]
  )(implicit registry: Registry): Verification = {
    implicit val fragment = target.position.fragment
    assert(!target.isInstanceOf[ConstructorDefinition] || classDefinition.isDefined)
    // TODO: Verify that the signature doesn't have two parameters with the same name.
    target.body.map { body =>
      val visitor = new FunctionVerificationVisitor(target, classDefinition)
      // TODO: Ensure that the return type matches the type of the body.
      (
        StmtVisitor.visit(visitor)(body),
        ReturnConstraints.verify(body),
      ).simultaneous
      // TODO: Assert that all nodes have been assigned a type.
    }.toCompiledOption.verification
  }
}
