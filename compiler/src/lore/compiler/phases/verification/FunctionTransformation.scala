package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{ConstructorDefinition, FunctionDefinition, FunctionSignature}
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.StmtVisitor

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
  * For a given function or constructor, builds a semantic expression tree from the body's abstract syntax tree.
  * It infers and checks expression types and checks all other constraints on expressions of that function's body.
  */
object FunctionTransformation {
  /**
    * Builds a semantic expression tree from the function body's abstract syntax tree. Infers and checks types of the
    * given function body and applies function transformations. Ensures that all other expression constraints hold.
    * Also ensures that the return type of the signature is sound compared to the type of the body.
    */
  def transform(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    function.bodyNode.map(bodyNode => transform(function.signature, function.typeScope, bodyNode, None)).toCompiledOption.map {
      body => function.body = body
    }
  }

  /**
    * Builds a semantic expression tree from the constructor body's abstract syntax tree. Infers and checks types of
    * the given constructor body and applies function transformations. Ensures that all other expression constraints
    * hold. Also ensures that constructor and construct calls are soundly typed.
    */
  def transform(constructor: ConstructorDefinition, classDefinition: ClassDefinition)(implicit registry: Registry): Verification = {
    transform(constructor.signature, constructor.typeScope, constructor.bodyNode, Some(classDefinition)).map {
      body => constructor.body = body.asInstanceOf[Expression.Block] // TODO: There has to be a better solution...
    }
  }

  private def transform(
    signature: FunctionSignature, typeScope: TypeScope, bodyNode: ExprNode,
    classDefinition: Option[ClassDefinition],
  )(implicit registry: Registry): Compilation[Expression] = {
    for {
      _ <- SignatureConstraints.verify(signature)
      _ <- ReturnConstraints.verify(bodyNode)
      visitor = new FunctionTransformationVisitor(signature, typeScope, classDefinition)
      body <- StmtVisitor.visit(visitor)(bodyNode)
      _ <- verifyOutputType(signature, body)
    } yield body
  }

  case class IllegallyTypedBody(signature: FunctionSignature, body: Expression) extends Error(signature.position) {
    override def message: String = s"The function ${signature.name} should return a value of type ${signature.outputType}, but actually returns" +
      s" a value of type ${body.tpe}."
  }

  /**
   * Verifies that the function's output type is compatible with the type of the body.
   */
  private def verifyOutputType(signature: FunctionSignature, body: Expression): Verification = {
    if (body.tpe <= signature.outputType) {
      Verification.succeed
    } else {
      Verification.fromErrors(List(IllegallyTypedBody(signature, body)))
    }
  }
}
