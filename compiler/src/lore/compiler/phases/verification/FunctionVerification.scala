package lore.compiler.phases.verification

import lore.compiler.syntax.{ExprNode, StmtNode}
import lore.compiler.syntax.visitor.{StmtVisitor, VerificationStmtVisitor}
import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.semantics.functions.{ConstructorDefinition, FunctionDefinition, FunctionSignature}
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.Type

// TODO: Maybe we should introduce two separate ASTs: The first one would be created by the parser and only
//       carry the position as immutable "state", while the second one would carry the other kinds of state
//       filled during the verification phase. THe FunctionVerificationVisitor wouldn't simply mutate state
//       of the AST but actually transform nodes of the first AST into the second AST. This can then also be
//       used to apply transformations in one swoop, allowing us to be extra sure that the transformation result
//       can actually be used by the parent. As it stands now, the burden lies with transformations to ensure
//       that they are not affecting the inferred type of the parent, for example.
//       Even more, we might be able to throw away some information only needed during verification, typing,
//       and transformation, omitting it from the second AST. This kind of intermediate representation would
//       remove the transpilation further from the actual syntax of the language. For example, constructor calls
//       and function calls (both syntactically SimpleCallNodes) could be differentiated, which would result in
//       a cleaner implementation of the transpiler. We could also remove some nodes outright, such as transforming
//       "greater than" expressions into "less than" expressions by flipping the arguments.

/**
  * For a given function or constructor, infers and checks expression types and checks all other constraints on
  * expressions of that function's body. Also applies function transformations. After the function checker has been
  * run without producing any compilation errors, we can be sure that all expressions are soundly typed and adhere
  * to all constraints (barring compiler bugs, of course).
  *
  * Ascribing inferred types is a side-effect of this verifier, as is reassigning the body node of a function
  * or constructor after transformation.
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
    * Infers and checks types of the given function body and applies function transformations. Ensures that
    * all other expression constraints hold. Also ensures that the return type of the signature is sound compared
    * to the type of the body.
    */
  def verifyTypeTransform(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    function.body.map(body => verifyTypeTransform(function.signature, function.typeScope, body, None)).toCompiledOption.map {
      body => function.body = body
    }
  }

  /**
    * Infers and checks types of the given constructor body and applies function transformations. Ensures that
    * all other expression constraints hold. Also ensures that constructor and construct calls are soundly typed.
    */
  def verifyTypeTransform(constructor: ConstructorDefinition, classDefinition: ClassDefinition)(implicit registry: Registry): Verification = {
    verifyTypeTransform(constructor.signature, constructor.typeScope, constructor.body, Some(classDefinition)).map {
      body => constructor.body = body.asInstanceOf[ExprNode.BlockNode] // TODO: There has to be a better solution...
    }
  }

  private def verifyTypeTransform(
    signature: FunctionSignature, typeScope: TypeScope, body: ExprNode,
    classDefinition: Option[ClassDefinition],
  )(implicit registry: Registry): Compilation[ExprNode] = {
    for {
      _ <- SignatureConstraints.verify(signature)
      _ <- {
        val visitor = new FunctionVerificationVisitor(signature, typeScope, classDefinition)
        (
          StmtVisitor.visit(visitor)(body),
          ReturnConstraints.verify(body),
        ).simultaneous.verification
      }
      body <- FunctionTransformations.transformBody(body)
      _ <- verifyOutputType(signature, body)
      _ <- assertTypesAssigned(body)
    } yield body
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
