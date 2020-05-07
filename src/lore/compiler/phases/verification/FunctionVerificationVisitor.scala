package lore.compiler.phases.verification

import lore.ast.ExprNode.AddressNode
import lore.ast.visitor.VerificationStmtVisitor
import lore.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.FunctionVerification.IllegallyTypedExpression
import lore.compiler.{Compilation, Fragment, Registry, TypeExpressionEvaluator}
import lore.definitions.FunctionSignature
import lore.types.{BasicType, ProductType, Subtyping, Type}

private[verification] class FunctionVerificationVisitor(
  /**
    * The signature of the function or constructor for which we want to infer types.
    */
  val signature: FunctionSignature,
)(implicit registry: Registry, fragment: Fragment) extends VerificationStmtVisitor {
  import ExprNode._
  import FunctionVerificationVisitor._
  import StmtNode._
  import TopLevelExprNode._

  /**
    * The function verification context used by the visitor to open/close scopes, register yields, and so on.
    */
  val context = new FunctionVerificationContext(signature)

  /**
    * Whether the given statement's inferred type is a subtype of one of the expected types.
    */
  private def havingSubtype(statement: StmtNode, expectedTypes: Type*): Verification = {
    if (!expectedTypes.exists(expected => Subtyping.isSubtype(statement.inferredType, expected))) {
      Compilation.fail(IllegallyTypedExpression(statement, expectedTypes.toList))
    } else Verification.succeed
  }

  private def beingNumber(statement: StmtNode): Verification = {
    havingSubtype(statement, BasicType.Int, BasicType.Real)
  }

  private def beingNumbers(statements: StmtNode*): Verification = {
    statements.toList.map(beingNumber).simultaneous.verification
  }

  private def beingBoolean(statement: StmtNode): Verification = {
    havingSubtype(statement, BasicType.Boolean)
  }

  private def beingBooleans(statements: StmtNode*): Verification = {
    statements.toList.map(beingBoolean).simultaneous.verification
  }

  private def typeBinaryNumbers(node: StmtNode, left: StmtNode, right: StmtNode): Verification = {
    beingNumbers(left, right).flatMap { _ =>
      if (left.inferredType == BasicType.Real || right.inferredType == BasicType.Real) {
        node.typed(BasicType.Real)
      } else { // Both operands are integers.
        node.typed(BasicType.Int)
      }
    }
  }

  private def typeXaryBooleans(node: StmtNode, nodes: List[StmtNode]): Verification = {
    beingBooleans(nodes: _*).flatMap { _ =>
      node.typed(BasicType.Boolean)
    }
  }

  override def verify(node: StmtNode): Verification = node match {
    // Literals.
    case RealLiteralNode(_)   => node.typed(BasicType.Real)
    case IntLiteralNode(_)    => node.typed(BasicType.Int)
    case BoolLiteralNode(_)   => node.typed(BasicType.Boolean)
    case StringLiteralNode(_) => node.typed(BasicType.String)
    case UnitNode             => node.typed(ProductType.UnitType)

    // Control nodes.
    case ReturnNode(expr) =>
      // TODO: Check that the returned expression adheres to the function's return type bounds.
      // TODO: Check that the return is the last statement in the block. This effectively disallows dead code after
      //       a return statement.
      // TODO: Disallow constructions such as `if ({ return 0 }) a else b`. Returning should not be possible from
      //       blocks that are in an expression position. We might have to add such a notion to blocks.
      // This doesn't quite adhere to the spec, but we'll go with unit for now.
      node.typed(ProductType.UnitType) // TODO: This should actually be a nothing/bottom type...
    case YieldNode(expr) =>
      // TODO: Yield is special in that it determines the result type of the surrounding loop. We must create a
      //       "loop context" that we add all instances of yield to; then we try to find the lowest common type bound
      //       for all expressions once the context comes back to the list.
      node.typed(ProductType.UnitType)

    // Variables.
    case node@VariableNode(name) =>
      // TODO: We will also need access to global variables if we introduce those into Lore.
      // TODO: Once we treat functions as values, we will have to make this even more complicated by also
      //       considering function names.
      context.currentScope.variable(name, node.position).flatMap { variable =>
        node.variable = variable
        node.typed(variable.tpe)
      }
    case node@PropertyAccessNode(instance, name) =>
      MemberExplorer.find(name, instance.inferredType, node.position).flatMap { member =>
        node.member = member
        node.typed(member.tpe)
      }
    case VariableDeclarationNode(name, isMutable, maybeTypeNode, value) =>
      // Add the variable type to the type context. Either infer the type from the value or, if a type has
      // been explicitly declared, check that the value adheres to the type bounds.
      maybeTypeNode.map { typeNode =>
        // Check that the value's inferred type adheres to the declared type bounds.
        TypeExpressionEvaluator.evaluate(typeNode).flatMap { tpe =>
          havingSubtype(value, tpe).map(_ => tpe)
        }
      }.toCompiledOption.map {
        // Now decide which type the variable should have.
        case None => value.inferredType
        case Some(tpe) => tpe
      }.flatMap { tpe =>
        // Register the local variable with the scope.
        val localVariable = LocalVariable(name, tpe, isMutable)
        context.currentScope.register(localVariable, node.position).flatMap { _ =>
          // An assignment always results in a unit value.
          node.typed(ProductType.UnitType)
        }
      }
    case AssignmentNode(address, value) =>
      // We check the assignment based on the kind of address node. Mostly, we want to ensure that the value
      // assigned to the variable adheres to its type bounds. We also ensure that the variable or property is
      // even assignable, i.e. mutable.
      val (tpe, isMutable) = address match {
        case variableNode: VariableNode => (variableNode.inferredType, variableNode.variable.isMutable)
        case accessNode: PropertyAccessNode => (accessNode.inferredType, accessNode.member.isMutable)
        case _ => throw new RuntimeException("This case should not be reached.")
      }
      (
        // Ensure that the value has the right type.
        havingSubtype(value, tpe),
        // Ensure that the variable or property is even mutable.
        Verification.fromErrors(if (!isMutable) ImmutableAssignment(address) :: Nil else Nil)
      ).simultaneous.flatMap(_ => node.typed(ProductType.UnitType))

    // Unary operations.
    case NegationNode(expr) =>
      beingNumber(expr).flatMap { _ =>
        node.typed(expr.inferredType)
      }
    case LogicalNotNode(expr) =>
      beingBoolean(expr).flatMap { _ =>
        node.typed(BasicType.Boolean)
      }

    // Repetitions.
    case RepeatWhileNode(condition, body, deferCheck) =>
      // TODO: Pop the latest yield context and build the list type.
      ???
    case IterationNode(extractors, body) =>
      // TODO: Do roughly the same as in the repeat-while. We also have to push a new scope to the context, though.
      ???

    // Binary operations.
    case AdditionNode(left, right) => typeBinaryNumbers(node, left, right)
    case SubtractionNode(left, right) => typeBinaryNumbers(node, left, right)
    case MultiplicationNode(left, right) => typeBinaryNumbers(node, left, right)
    case DivisionNode(left, right) => typeBinaryNumbers(node, left, right)
    case EqualsNode(left, right) =>
      // TODO: Should we check the input types here at all? For example that types must be equal? Or at least in a
      //       subtyping relationship with each other?
      node.typed(BasicType.Boolean)
    case NotEqualsNode(left, right) => node.typed(BasicType.Boolean)
    case LessThanNode(left, right) => node.typed(BasicType.Boolean)
    case LessThanEqualsNode(left, right) => node.typed(BasicType.Boolean)
    case GreaterThanNode(left, right) => node.typed(BasicType.Boolean)
    case GreaterThanEqualsNode(left, right) => node.typed(BasicType.Boolean)

    // Ternary
    case IfElseNode(condition, onTrue, onFalse) =>
      havingSubtype(condition, BasicType.Boolean).flatMap { _ =>
        // TODO: If only one branch supplies a value, return an OPTION of the evaluated type. Of course, we don't
        //       HAVE options just yet.
        val resultType = Subtyping.leastUpperBound(onTrue.inferredType, onFalse.inferredType)
        node.typed(resultType)
      }

    // Constructors.
    case ConstructorCallNode(name, arguments) => ???
    case ConstructNode(arguments, withSuper) => ???

    // Blocks.
    case BlockNode(statements) => ???

    // Literal constructions.
    case TupleNode(expressions) => ???
    case ListNode(expressions) => ???
    case MapNode(entries) => ???

    // Xary operations.
    case ConjunctionNode(expressions) => typeXaryBooleans(node, expressions)
    case DisjunctionNode(expressions) => typeXaryBooleans(node, expressions)
    case ConcatenationNode(expressions) => ???
    case CallNode(name, qualifier, arguments) => ???
    case FixedFunctionCallNode(name, types, arguments) => ???
  }

  override def before: PartialFunction[StmtNode, Unit] = {
    case ExprNode.RepeatWhileNode(_, _, _) | ExprNode.IterationNode(_, _) =>
      // TODO: Put a new yield context on the stack.
    // TODO: In case of a block opening: Put a new variable scope on the stack.
  }
}

private[verification] object FunctionVerificationVisitor {
  case class ImmutableAssignment(addressNode: AddressNode)(implicit fragment: Fragment) extends Error(addressNode) {
    override def message = s"The variable or property you are trying to assign to is immutable."
  }

  implicit class StmtNodeExtension(node: StmtNode) {
    /**
      * Assigns the given type to the node and returns the type.
      */
    def typed(tpe: Type): Verification = {
      node.setInferredType(tpe)
      Verification.succeed
    }
  }
}
