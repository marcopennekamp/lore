package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core.{Compilation, CompilationException, Error, Errors, Position, Result}
import lore.compiler.phases.resolution.TypeExpressionEvaluator
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.{LocalVariable, Registry, TypeScope, VariableScope}
import lore.compiler.syntax.visitor.StmtVisitor
import lore.compiler.syntax.{ExprNode, StmtNode, TopLevelExprNode, TypeExprNode}
import lore.compiler.types._

private[transformation] class ExpressionTransformationVisitor(
  /**
    * The expected result type of the transformed expression. Note that this is merely used to check the result
    * type of a return expression. It does not guarantee that the visitor returns an expression with the given
    * type. That needs to be checked separately.
    */
  expectedType: Type,

  /**
    * The type scope of the surrounding code, such as a function's type scope.
    */
  typeScope: TypeScope,

  /**
    * The variable scope of the surrounding code, such as a function scope.
    */
  variableScope: VariableScope,
)(implicit registry: Registry) extends StmtVisitor[Expression] {
  import ExprNode._
  import ExpressionTransformationVisitor._
  import StatementTransformation._
  import StmtNode._
  import TopLevelExprNode._

  val context = new ExpressionTransformationContext(variableScope)
  implicit val typeScopeImplicit: TypeScope = typeScope

  override def visitLeaf(node: LeafNode): Compilation[Expression] = node match {
    case VariableNode(name, _) =>
      implicit val position: Position = node.position
      context.currentScope.resolve(name).map(Expression.VariableAccess(_, position))

    case RealLiteralNode(value, position) => Expression.Literal(value, BasicType.Real, position).compiled
    case node@IntLiteralNode(value, position) =>
      if (BasicType.Int.minSafeInteger <= value && value <= BasicType.Int.maxSafeInteger) {
        Expression.Literal(value, BasicType.Int, position).compiled
      } else {
        Compilation.fail(UnsafeInteger(node))
      }
    case BoolLiteralNode(value, position) => Expression.Literal(value, BasicType.Boolean, position).compiled
    case StringLiteralNode(value, position) => Expression.Literal(value, BasicType.String, position).compiled
    case UnitNode(position) => Expression.Tuple(Vector.empty, position).compiled
  }

  override def visitUnary(node: UnaryNode)(expression: Expression): Compilation[Expression] = node match {
    case ReturnNode(_, position) =>
      ExpressionVerification.hasSubtype(expression, expectedType).map(_ => Expression.Return(expression, position))

    case VariableDeclarationNode(name, isMutable, maybeTypeExpr, _, _) =>
      implicit val position: Position = node.position

      // Either infer the type from the value or, if a type has been explicitly declared, check that the value adheres
      // to the type bounds.
      val typeAnnotation = maybeTypeExpr.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      val inferredType = typeAnnotation.flatMap {
        case Some(tpe) => ExpressionVerification.hasSubtype(expression, tpe).map(_ => tpe)
        case None => expression.tpe.compiled
      }

      inferredType match {
        case Result(_, _) => inferredType.map { tpe =>
          val variable = LocalVariable(name, tpe, isMutable)
          context.currentScope.register(variable)
          Expression.VariableDeclaration(variable, expression, position)
        }
        case compilation@Errors(_, _) =>
          // Add a variable with the correct type if the assignment's type check goes wrong. Currently, if a variable
          // declaration is incorrect, subsequent uses of the variable also lead to a confusing "this scope does not
          // know entry x" error. The entry should be known. Its type or expression is just wrong for the time being.
          context.currentScope.register(
            LocalVariable(name, typeAnnotation.map(_.getOrElse(expression.tpe)).getOrElse(BasicType.Any), isMutable)
          )
          compilation.asInstanceOf[Errors[Expression]]
      }

    case NegationNode(_, position) =>
      ExpressionVerification.isNumeric(expression).map(_ => Expression.UnaryOperation(UnaryOperator.Negation, expression, expression.tpe, position))
    case LogicalNotNode(_, position) =>
      ExpressionVerification.isBoolean(expression).map(_ => Expression.UnaryOperation(UnaryOperator.LogicalNot, expression, BasicType.Boolean, position))

    case MemberAccessNode(_, name, _) =>
      implicit val position: Position = node.position
      expression.tpe.member(name).map(member => Expression.MemberAccess(expression, member, position))
  }

  override def visitBinary(node: BinaryNode)(left: Expression, right: Expression): Compilation[Expression] = node match {
    case AssignmentNode(_, _, position) =>
      // We check the assignment based on the kind of address node. Mostly, we want to ensure that the value
      // assigned to the variable adheres to its type bounds. We also ensure that the variable or property is
      // even assignable, i.e. mutable.
      val access = left match {
        case access: Expression.Access => access
        case _ => throw CompilationException("The left-hand-side expression of an assignment node must be an Access expression.")
      }
      val value = right
      val (tpe, isMutable) = access match {
        case variableAccess: Expression.VariableAccess => (variableAccess.tpe, variableAccess.variable.isMutable)
        case memberAccess: Expression.MemberAccess => (memberAccess.tpe, memberAccess.member.isAssignable)
      }
      for {
        // Ensure that the variable or member is even mutable.
        _ <- Verification.fromErrors(if (!isMutable) Vector(ImmutableAssignment(access)) else Vector.empty)
        // Ensure that the value has the right type.
        _ <- ExpressionVerification.hasSubtype(value, tpe)
      } yield Expression.Assignment(access, value, position)

    // Arithmetic operations.
    case AdditionNode(_, _, position) => transformNumericOperation(BinaryOperator.Addition, left, right, position)
    case SubtractionNode(_, _, position) => transformNumericOperation(BinaryOperator.Subtraction, left, right, position)
    case MultiplicationNode(_, _, position) => transformNumericOperation(BinaryOperator.Multiplication, left, right, position)
    case DivisionNode(_, _, position) => transformNumericOperation(BinaryOperator.Division, left, right, position)

    // Boolean operations.
    case EqualsNode(_, _, position) =>
      StatementTransformation.transformComparison("areEqual", BinaryOperator.Equals, left, right, position)
    case NotEqualsNode(_, _, position) =>
      StatementTransformation.transformComparison("areEqual", BinaryOperator.Equals, left, right, position).map {
        areEqual => Expression.UnaryOperation(UnaryOperator.LogicalNot, areEqual, BasicType.Boolean, position)
      }
    case LessThanNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThan", BinaryOperator.LessThan, left, right, position)
    case LessThanEqualsNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThanOrEqual", BinaryOperator.LessThanEquals, left, right, position)
    case GreaterThanNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThan", BinaryOperator.LessThan, right, left, position)
    case GreaterThanEqualsNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThanOrEqual", BinaryOperator.LessThanEquals, right, left, position)

    // Collection operations.
    case AppendNode(_, _, position) =>
      left.tpe match {
        case ListType(elementType) =>
          // The immutable list is constructed from the existing list and another element. The resulting type of the
          // list will be the least upper bound of the two types. If the two types aren't related, we default to sum
          // types, which provide a sensible default for complex list construction.
          val combinedType = LeastUpperBound.leastUpperBound(elementType, right.tpe)
          Expression.BinaryOperation(BinaryOperator.Append, left, right, ListType(combinedType), position).compiled
        case _ => Compilation.fail(ExpressionVerification.IllegallyTypedExpression(left, Vector(ListType(BasicType.Any))))
      }

    // Loops.
    case WhileNode(_, _, position) =>
      // Close the previously opened scope.
      context.closeScope()
      val condition = left
      val body = right
      for {
        _ <- ExpressionVerification.isBoolean(condition)
        tpe <- ExpressionVerification.inferLoopType(body)
      } yield Expression.WhileLoop(condition, body, tpe, position)
  }

  override def visitTernary(node: TernaryNode)(
    argument1: Expression, argument2: Expression, argument3: Expression,
  ): Compilation[Expression] = node match {
    case IfElseNode(_, _, _, position) =>
      val condition = argument1
      val onTrue = argument2
      val onFalse = argument3
      ExpressionVerification.isBoolean(condition).map { _ =>
        val tpe = LeastUpperBound.leastUpperBound(onTrue.tpe, onFalse.tpe)
        Expression.IfElse(condition, onTrue, onFalse, tpe, position)
      }
  }

  override def visitXary(node: XaryNode)(expressions: Vector[Expression]): Compilation[Expression] = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      context.closeScope()
      Expression.Block(expressions, position).compiled

    // Value constructors.
    case TupleNode(_, position) =>
      Expression.Tuple(expressions, position).compiled

    case ListNode(_, position) =>
      // If we type empty lists as [Nothing], we can assign this empty list to any kind of list, which makes
      // coders happy. :) Hence the default value in the fold.
      val elementType = LeastUpperBound.leastUpperBound(expressions.map(_.tpe))
      Expression.ListConstruction(expressions, ListType(elementType), position).compiled

    case ObjectMapNode(structName, entryNodes, position) =>
      implicit val pos: Position = position
      registry.resolveType(structName).flatMap {
        case structType: StructType =>
          val entries = entryNodes.zip(expressions).map { case (ObjectEntryNode(name, _, _), expression) => (name, expression) }
          InstantiationTransformation.transformMapStyleInstantiation(structType.definition, entries)
        case _ => Compilation.fail(StructExpected(structName))
      }

    // Xary operations.
    case ConjunctionNode(_, position) =>
      StatementTransformation.transformBooleanOperation(XaryOperator.Conjunction, expressions, position)

    case DisjunctionNode(_, position) =>
      StatementTransformation.transformBooleanOperation(XaryOperator.Disjunction, expressions, position)

    case ConcatenationNode(_, position) =>
      for {
        transformedExpressions <- expressions.map { expression =>
          if (expression.tpe == BasicType.String) expression.compiled
          else ExpressionBuilder.multiFunctionCall("toString", Vector(expression), expression.position)
        }.simultaneous
      } yield Expression.XaryOperation(XaryOperator.Concatenation, transformedExpressions, BasicType.String, position)

    // Function calls.
    case SimpleCallNode(name, _, position) =>
      implicit val pos: Position = position
      // A simple call may either be a function or a constructor call. We immediately try to differentiate this based
      // on whether a struct type can be found for the function name.
      registry.getStructType(name) match {
        case Some(structType) => InstantiationTransformation.transformCallStyleInstantiation(structType.definition, expressions)
        case None => ExpressionBuilder.multiFunctionCall(name, expressions, position)
      }

    case FixedFunctionCallNode(name, typeExpressions, _, position) =>
      for {
        types <- typeExpressions.map(TypeExpressionEvaluator.evaluate).simultaneous
        function <- registry.resolveExactFunction(name, types)(position)
        instance <- function.instantiate(ProductType(types))
      } yield Expression.Call(instance, expressions, position)

    case DynamicCallNode(resultType, _, position) =>
      (
        // The first argument to the dynamic call must be a constant function name.
        expressions.headOption match {
          case Some(Expression.Literal(name: String, BasicType.String, _)) => name.compiled
          case _ => Compilation.fail(DynamicFunctionNameExpected()(position))
        },
        TypeExpressionEvaluator.evaluate(resultType),
      ).simultaneous.map { case (name, resultType) =>
        Expression.Call(DynamicCallTarget(name, resultType), expressions.tail, position)
      }
  }

  override def visitMap(node: MapNode)(kvs: Vector[(Expression, Expression)]): Compilation[Expression] = {
    val entries = kvs.map(Expression.MapEntry.tupled)
    val keyType = LeastUpperBound.leastUpperBound(entries.map(_.key.tpe))
    val valueType = LeastUpperBound.leastUpperBound(entries.map(_.value.tpe))
    Expression.MapConstruction(entries, MapType(keyType, valueType), node.position).compiled
  }

  override def visitIteration(node: ForNode)(
    extractorTuples: Vector[(String, Expression)], visitBody: () => Compilation[Expression],
  ): Compilation[Expression] = {
    // Before we visit the body, we have to push a new scope and later, once extractors have been evaluated, also
    // a new loop context.
    context.openScope()
    val scope = context.currentScope

    def transformExtractor(variableName: String, collection: Expression, position: Position): Compilation[Expression.Extractor] = {
      for {
        elementType <- collection.tpe match {
          case ListType(element) => Compilation.succeed(element)
          case MapType(key, value) => Compilation.succeed(ProductType(Vector(key, value)))
          case _ => Compilation.fail(CollectionExpected(collection))
        }
        localVariable = LocalVariable(variableName, elementType, isMutable = false)
        _ <- scope.register(localVariable)(position)
      } yield Expression.Extractor(localVariable, collection)
    }

    for {
      extractors <- extractorTuples.zip(node.extractors.map(_.position)).map {
        case ((variableName, collection), position) => transformExtractor(variableName, collection, position)
      }.simultaneous
      body <- visitBody()
      _ = context.closeScope() // We have to close the scope that we opened for the extractors.
      tpe <- ExpressionVerification.inferLoopType(body)
    } yield Expression.ForLoop(extractors, body, tpe, node.position)
  }

  override def before: PartialFunction[StmtNode, Unit] = {
    case ExprNode.BlockNode(_, _) => context.openScope()
    case ExprNode.WhileNode(_, _, _) =>
      // A while loop needs to open its own scope in case there is exactly one variable declaration as the loop body,
      // which wouldn't get scoped by the block.
      context.openScope()
  }
}

private[transformation] object ExpressionTransformationVisitor {
  case class UnsafeInteger(node: ExprNode.IntLiteralNode) extends Error(node) {
    override def message: String = s"The integer literal ${node.value} is outside the safe run-time range of" +
      s" ${BasicType.Int.minSafeInteger} and ${BasicType.Int.maxSafeInteger}. The Javascript runtime will not be able" +
      s" to properly store and process integers this large."
  }

  case class ImmutableAssignment(access: Expression.Access) extends Error(access) {
    override def message = s"The variable or member ${access.name} you are trying to assign to is immutable."
  }

  case class DynamicFunctionNameExpected()(implicit position: Position) extends Error(position) {
    override def message: String = "Dynamic calls require a string literal as their first argument, which represents the" +
      " name of the function. Since the name must be available at compile-time, it must be a constant."
  }

  case class CollectionExpected(expression: Expression) extends Error(expression) {
    override def message: String = s"Expected a collection at this position. Got a value of type ${expression.tpe}."
  }

  case class StructExpected(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The name $name must refer to a struct."
  }
}
