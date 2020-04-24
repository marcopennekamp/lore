package lore.compiler.phases.verification

import lore.ast.visitor.StmtVisitor
import lore.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.{Compilation, Fragment, Registry}
import lore.types.{BasicType, ProductType, Type}

/**
  * Infers and checks expression types. After the type checker has been run without producing any compilation
  * errors, we can be sure that all expressions are soundly typed (barring compiler bugs, of course).
  */
object TypeChecker {
  case class IllegallyTypedExpression(expr: StmtNode, expectedTypes: List[Type])(implicit fragment: Fragment) extends Error(expr) {
    override def message = s"The expression $expr has the illegal type ${expr.inferredType}$expected."
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s"We expected one of the following types: ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  class TypeCheckVisitor()(implicit registry: Registry, fragment: Fragment) extends StmtVisitor[Type] {
    import ExprNode._
    import StmtNode._
    import TopLevelExprNode._

    implicit class StmtNodeExtension(node: StmtNode) {
      /**
        * Assigns the given type to the node and returns the type.
        */
      def typed(tpe: Type): Compilation[Type] = {
        node.setInferredType(tpe)
        Compilation.succeed(tpe)
      }
    }

    private def havingType(statement: StmtNode, expectedTypes: Type*): Verification = {
      if (!expectedTypes.contains(statement.inferredType)) {
        Compilation.fail(IllegallyTypedExpression(statement, expectedTypes.toList))
      } else Verification.succeed
    }

    private def beingNumber(statement: StmtNode): Verification = {
      havingType(statement, BasicType.Int, BasicType.Real)
    }

    private def beingNumbers(statements: StmtNode*): Verification = {
      statements.toList.map(beingNumber).simultaneous.verification
    }

    private def beingBoolean(statement: StmtNode): Verification = {
      havingType(statement, BasicType.Boolean)
    }

    private def beingBooleans(statements: StmtNode*): Verification = {
      statements.toList.map(beingBoolean).simultaneous.verification
    }

    override def visitLeaf(node: StmtNode.LeafNode): Compilation[Type] = node match {
      // Variables.
      case VariableNode(name) =>
        // TODO: We need a sort of type context that saves local variable types etc. to resolve this.
        ???

      // Literals.
      case RealLiteralNode(_)   => node.typed(BasicType.Real)
      case IntLiteralNode(_)    => node.typed(BasicType.Int)
      case BoolLiteralNode(_)   => node.typed(BasicType.Boolean)
      case StringLiteralNode(_) => node.typed(BasicType.String)
      case UnitNode             => node.typed(ProductType.UnitType)
    }

    override def visitUnary(node: StmtNode.UnaryNode)(exprType: Type): Compilation[Type] = node match {
      // Control nodes.
      case StmtNode.ReturnNode(expr) =>
        // TODO: Check that the returned expression adheres to the function's return type bounds.
        // This doesn't quite adhere to the spec, but we'll go with unit for now.
        // TODO: This should actually be a nothing/bottom type...
        node.typed(ProductType.UnitType)
      case TopLevelExprNode.YieldNode(expr) =>
        // TODO: Yield is special in that it determines the result type of the surrounding loop. We must create a
        //       "loop context" that we add all instances of yield to; then we try to find the lowest common type bound
        //       for all expressions once the context comes back to the list.
        node.typed(ProductType.UnitType)

      // Variable nodes.
      case TopLevelExprNode.VariableDeclarationNode(name, isMutable, tpe, value) =>
        // TODO: Add the variable type to the type context. Either infer the type from the value or, if a type has
        //       been explicitly declared, check that the value adheres to the type bounds.
        node.typed(ProductType.UnitType)
      case TopLevelExprNode.AssignmentNode(address, value) =>
        // TODO: Check that the value assigned to the variable adheres to the variable's type bounds.
        node.typed(ProductType.UnitType)

      // Unary operations.
      case ExprNode.NegationNode(expr) =>
        beingNumber(expr).flatMap { _ =>
          node.typed(expr.inferredType)
        }
      case ExprNode.LogicalNotNode(expr) =>
        beingBoolean(expr).flatMap { _ =>
          node.typed(BasicType.Boolean)
        }
      case ExprNode.PropertyAccessNode(instance, names) =>
        // TODO: The instance must be a class. Then we also need to get the actual property type at the end of
        //       the chain. So we not only have to infer types here, but also report an error if the property
        //       doesn't exist.
        ???
    }

    override def visitBinary(node: BinaryNode)(leftType: Type, rightType: Type): Compilation[Type] = {
      def typeNumbers(left: StmtNode, right: StmtNode): Compilation[Type] = {
        beingNumbers(left, right).flatMap { _ =>
          if (left.inferredType == BasicType.Real || right.inferredType == BasicType.Real) {
            node.typed(BasicType.Real)
          } else { // Both operands are integers.
            node.typed(BasicType.Int)
          }
        }
      }

      node match {
        // Repetitions.
        case RepeatWhileNode(condition, body, deferCheck) =>
          // TODO: Pop the latest yield context and build the list type.
          ???

        // Binary operations.
        case AdditionNode(left, right) => typeNumbers(left, right)
        case SubtractionNode(left, right) => typeNumbers(left, right)
        case MultiplicationNode(left, right) => typeNumbers(left, right)
        case DivisionNode(left, right) => typeNumbers(left, right)
        case EqualsNode(left, right) =>
          // TODO: Should we check the input types here at all?
          node.typed(BasicType.Boolean)
        case NotEqualsNode(left, right) => node.typed(BasicType.Boolean)
        case LessThanNode(left, right) => node.typed(BasicType.Boolean)
        case LessThanEqualsNode(left, right) => node.typed(BasicType.Boolean)
        case GreaterThanNode(left, right) => node.typed(BasicType.Boolean)
        case GreaterThanEqualsNode(left, right) => node.typed(BasicType.Boolean)
      }
    }

    override def visitTernary(node: TernaryNode)(type1: Type, type2: Type, type3: Type): Compilation[Type] = node match {
      case IfElseNode(condition, onTrue, onFalse) =>
        // TODO: Check that condition is boolean.
        // TODO: Calculate the lowest type bound of onTrue and onFalse and make that the type of the if-else node.
        ???
    }

    override def visitXary(node: XaryNode)(argumentTypes: List[Type]): Compilation[Type] = {
      def typeBooleans(nodes: List[StmtNode]): Compilation[Type] = {
        beingBooleans(nodes: _*).flatMap { _ =>
          node.typed(BasicType.Boolean)
        }
      }

      node match {
        // Constructors.
        case ConstructorCallNode(name, arguments) => ???
        case ConstructNode(arguments, withSuper) => ???

        // Blocks.
        case BlockNode(statements) => ???

        // Literal constructions.
        case TupleNode(expressions) => ???
        case ListNode(expressions) => ???

        // Xary operations.
        case ConjunctionNode(expressions) => typeBooleans(expressions)
        case DisjunctionNode(expressions) => typeBooleans(expressions)
        case ConcatenationNode(expressions) => ???
        case CallNode(name, qualifier, arguments) => ???
        case FixedFunctionCallNode(name, types, arguments) => ???
      }
    }

    override def visitMap(node: MapNode)(entryTypes: List[(Type, Type)]): Compilation[Type] = {
      ???
    }

    override def visitIteration(node: IterationNode)(extractors: List[(String, Type)], bodyType: Type): Compilation[Type] = {
      ???
    }

    override def before: PartialFunction[StmtNode, Unit] = {
      case ExprNode.RepeatWhileNode(_, _, _) | ExprNode.IterationNode(_, _) =>
      // TODO: Put a new yield context on the stack.
    }
  }

  def infer(expression: StmtNode)(implicit registry: Registry, fragment: Fragment): Type = {
    ???
  }
}
