package lore.compiler.phases.verification

import lore.compiler.ast.StmtNode
import lore.compiler.ast.visitor.{StmtVisitor, VerificationStmtVisitor}
import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{CompilationException, Fragment, Registry, TypeScope}
import lore.compiler.feedback.Error
import lore.compiler.functions.{ConstructorDefinition, FunctionDefinition, FunctionSignature}
import lore.compiler.structures.ClassDefinition
import lore.compiler.types.Type

/**
  * For a given function or constructor, infers and checks expression types and checks all other constraints on
  * expressions of that function's body. After the function checker has been run without producing any compilation
  * errors, we can be sure that all expressions are soundly typed and adhere to all constraints (barring compiler bugs,
  * of course).
  *
  * Ascribing inferred types is a side-effect of this verifier.
  */
object FunctionVerification {
  case class IllegallyTypedExpression(expr: StmtNode, expectedTypes: List[Type]) extends Error(expr) {
    override def message = s"The expression $expr has the illegal type ${expr.state.inferredType}.$expected"
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types (or a subtype thereof): ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  case class IllegallyTypedBody(signature: FunctionSignature, body: StmtNode) extends Error(signature.position) {
    override def message: String = s"The function ${signature.name} should return a value of type ${signature.outputType}, but actually returns" +
      s" a value of type ${body.state.inferredType}."
  }

  /**
    * Infers and checks types of the given function body. Ensures that all other expression constraints hold.
    * Also ensures that the return type of the signature is sound compared to the type of the body.
    */
  def verifyFunction(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    function.body.map(body => verify(function.signature, function.typeScope, body, None)).toCompiledOption.verification
  }

  /**
    * Infers and checks types of the given constructor body. Ensures that all other expression constraints
    * hold. Also ensures that constructor and construct calls are soundly typed.
    */
  def verifyConstructor(
    constructor: ConstructorDefinition,
    classDefinition: ClassDefinition,
  )(implicit registry: Registry): Verification = {
    verify(constructor.signature, constructor.typeScope, constructor.body, Some(classDefinition))
  }

  private def verify(
    signature: FunctionSignature, typeScope: TypeScope, body: StmtNode,
    classDefinition: Option[ClassDefinition],
  )(implicit registry: Registry): Verification = {
    for {
      _ <- SignatureConstraints.verify(signature)
      _ <- {
        val visitor = new FunctionVerificationVisitor(signature, typeScope, classDefinition)
        (
          StmtVisitor.visit(visitor)(body),
          ReturnConstraints.verify(body),
        ).simultaneous.verification
      }
      _ <- verifyOutputType(signature, body)
      _ <- assertTypesAssigned(body)
    } yield ()
  }

  /**
   * Verifies that the function's output type is compatible with the type of the body.
   */
  private def verifyOutputType(signature: FunctionSignature, body: StmtNode): Verification = {
    if (body.state.inferredType <= signature.outputType) {
      Verification.succeed
    } else {
      Verification.fromErrors(List(IllegallyTypedBody(signature, body)))
    }
  }

  /**
   * Asserts that every node in the body has been assigned a type. If this is not the case, we have a compiler bug.
   */
  private def assertTypesAssigned(body: StmtNode): Verification = {
    StmtVisitor.visit(new VerificationStmtVisitor {
      override def verify(node: StmtNode): Verification = {
        try {
          node.state.inferredType
          Verification.succeed
        } catch {
          case _: CompilationException => throw CompilationException(
            s"The node $node at ${node.position} does not have an inferred type assigned to itself."
          )
        }
      }
    })(body)
  }
}
