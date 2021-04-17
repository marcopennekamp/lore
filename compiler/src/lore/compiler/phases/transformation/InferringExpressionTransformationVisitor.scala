package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core._
import lore.compiler.phases.resolution.TypeExpressionEvaluator
import lore.compiler.phases.transformation.InferringExpressionTransformationVisitor._
import lore.compiler.phases.transformation.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.scopes.{LocalVariable, TypeScope, TypedVariable, VariableScope}
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.types._

class InferringExpressionTransformationVisitor(
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
)(implicit registry: Registry) extends TopLevelExprVisitor[Expression, Compilation] {

  import ExprNode._
  import TopLevelExprNode._

  val scopeContext = new ScopeContext(variableScope)
  implicit val typeScopeImplicit: TypeScope = typeScope

  // TODO: In general, typing judgments only have to be added if the expression's type is not yet fully inferred.
  //       For example, when we add a typing judgment for a LogicalNot, if the child expression's type is already
  //       known, we can evaluate the judgment immediately to avoid clogging the type inference algorithm.
  //       The important idea here is that we can evaluate this when adding judgments, so that we don't have to rewrite
  //       the code for each node. It can happen "automagically".
  var typingJudgments: Vector[TypingJudgment] = Vector.empty

  override def visitLeaf(node: LeafNode): Compilation[Expression] = node match {
    case VariableNode(name, _) =>
      implicit val position: Position = node.position
      scopeContext.currentScope.resolve(name).map {
        case mf: MultiFunctionDefinition =>
          // Multi-functions which aren't used in a simple call must be converted to function values immediately.
          val functionType = new InferenceVariable
          typingJudgments = typingJudgments :+ TypingJudgment.MultiFunctionValue(functionType, mf, position)
          Expression.MultiFunctionValue(mf, functionType, position)

        case variable: TypedVariable => Expression.VariableAccess(variable, position)
      }

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
      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(expression.tpe, expectedType, position)
      Expression.Return(expression, position).compiled

    case VariableDeclarationNode(name, isMutable, maybeTypeExpr, _, _) =>
      implicit val position: Position = node.position

      // Either infer the type from the value or, if a type has been explicitly declared, check that the value adheres
      // to the type bounds.
      val typeAnnotation = maybeTypeExpr.map(TypeExpressionEvaluator.evaluate).toCompiledOption
      val inferredType = typeAnnotation.map {
        case Some(tpe) =>
          typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(expression.tpe, tpe, position)
          tpe
        case None => expression.tpe
      }

      inferredType.map { tpe =>
        val variable = LocalVariable(name, new InferenceVariable, isMutable)
        scopeContext.currentScope.register(variable)
        typingJudgments = typingJudgments :+ TypingJudgment.Assign(variable.tpe, tpe, position)
        Expression.VariableDeclaration(variable, expression, position)
      }

    case NegationNode(_, position) =>
      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(expression.tpe, BasicType.Real, position)
      Expression.UnaryOperation(UnaryOperator.Negation, expression, expression.tpe, position).compiled
    case LogicalNotNode(_, position) =>
      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(expression.tpe, BasicType.Boolean, position)
      Expression.UnaryOperation(UnaryOperator.LogicalNot, expression, BasicType.Boolean, position).compiled

    case MemberAccessNode(_, name, position) =>
      // TODO: A member only needs to be resolved lazily if the expression's type is not yet inferred. The expression's
      //       type will be clear in the majority of cases, so here is a chance to save on a lot of typing judgments
      //       if we check whether the expression actually even needs to be inferred.

      // We cannot decide the member until the type has been inferred. Hence we first have to return an "unresolved
      // member access" expression node, which will be resolved later.
      val memberType = new InferenceVariable
      typingJudgments = typingJudgments :+ TypingJudgment.MemberAccess(memberType, expression.tpe, name, position)
      Expression.UnresolvedMemberAccess(expression, name, memberType, position).compiled

    case AnonymousFunctionNode(parameterNodes, _, position) =>
      val parameters = parameterNodes.map { case AnonymousFunctionParameterNode(name, _, position) =>
        // TODO: The fact that we have to resolve the variable again, despite it being declared within the before hook,
        //       is not optimal. Maybe we can come up with a better solution here.
        val variable = scopeContext.currentScope.get(name).getOrElse(
          throw CompilationException(s"The anonymous function parameter at $position should have a corresponding scope entry."),
        ).asInstanceOf[TypedVariable]
        Expression.AnonymousFunctionParameter(name, variable.tpe, position)
      }
      scopeContext.closeScope()
      Expression.AnonymousFunction(parameters, expression, position).compiled
  }

  override def visitBinary(node: BinaryNode)(left: Expression, right: Expression): Compilation[Expression] = node match {
    case AssignmentNode(_, _, position) =>
      // Ensure that the value on the right can be assigned to the variable or member on the left.
      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(right.tpe, left.tpe, position)

      val access = left match {
        case access: Expression.Access => access
        case _ => throw CompilationException("The left-hand-side expression of an assignment must be a variable or a member.")
      }
      Expression.Assignment(access, right, position).compiled

    // Arithmetic operations.
    case AdditionNode(_, _, position) => transformNumericOperation(BinaryOperator.Addition, left, right, position).compiled
    case SubtractionNode(_, _, position) => transformNumericOperation(BinaryOperator.Subtraction, left, right, position).compiled
    case MultiplicationNode(_, _, position) => transformNumericOperation(BinaryOperator.Multiplication, left, right, position).compiled
    case DivisionNode(_, _, position) => transformNumericOperation(BinaryOperator.Division, left, right, position).compiled

    // Boolean operations.
    case EqualsNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Equals, left, right, BasicType.Boolean, position).compiled
    case NotEqualsNode(_, _, position) =>
      Expression.UnaryOperation(
        UnaryOperator.LogicalNot,
        Expression.BinaryOperation(BinaryOperator.Equals, left, right, BasicType.Boolean, position),
        BasicType.Boolean,
        position,
      ).compiled
    case LessThanNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThan, left, right, BasicType.Boolean, position).compiled
    case LessThanEqualsNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThanEquals, left, right, BasicType.Boolean, position).compiled
    case GreaterThanNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThan, right, left, BasicType.Boolean, position).compiled
    case GreaterThanEqualsNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThanEquals, right, left, BasicType.Boolean, position).compiled

    // Collection operations.
    case AppendNode(_, _, position) =>
      // TODO: If append also has to work for maps, we might have to rewrite these rules or even use alternative judgments.
      val elementType = new InferenceVariable
      val combinedType = new InferenceVariable

      // The Equals judgment is chosen deliberately, because we want the list's type to be able to be inferred from the
      // combined type (which might in turn be inferred from an explicitly typed variable declaration). This inference
      // is possible due to the bidirectionality of the LUB judgment.
      typingJudgments = typingJudgments :+ TypingJudgment.Equals(ListType(elementType), left.tpe, position)
      typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(combinedType, Vector(elementType, right.tpe), position)

      Expression.BinaryOperation(BinaryOperator.Append, left, right, ListType(combinedType), position).compiled

    // Loops.
    case WhileNode(_, _, position) =>
      // Close the previously opened scope.
      scopeContext.closeScope()
      val condition = left
      val body = right

      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(condition.tpe, BasicType.Boolean, position)

      Expression.WhileLoop(condition, body, inferLoopType(body, position), position).compiled
  }

  private def transformNumericOperation(
    operator: BinaryOperator,
    left: Expression,
    right: Expression,
    position: Position,
  ): Expression.BinaryOperation = {
    // TODO: If both types are already inferred, we don't need to add any typing judgments. (Similar to how members
    //       only need to be inferred if their instance expression is not yet inferred.)
    typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(left.tpe, BasicType.Real, position)
    typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(right.tpe, BasicType.Real, position)
    val resultType = new InferenceVariable
    typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(resultType, Vector(left.tpe, right.tpe), position)

    Expression.BinaryOperation(operator, left, right, resultType, position)
  }

  override def visitTernary(node: TernaryNode)(
    argument1: Expression, argument2: Expression, argument3: Expression,
  ): Compilation[Expression] = node match {
    case IfElseNode(_, _, _, position) =>
      val condition = argument1
      val onTrue = argument2
      val onFalse = argument3
      val resultType = new InferenceVariable

      typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(condition.tpe, BasicType.Boolean, position)
      typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(resultType, Vector(onTrue.tpe, onFalse.tpe), position)

      Expression.IfElse(condition, onTrue, onFalse, resultType, position).compiled
  }

  override def visitXary(node: XaryNode)(expressions: Vector[Expression]): Compilation[Expression] = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      scopeContext.closeScope()
      Expression.Block(expressions, position).compiled

    // Value constructors.
    case TupleNode(_, position) =>
      Expression.Tuple(expressions, position).compiled

    case ListNode(_, position) =>
      val elementType = new InferenceVariable
      typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(elementType, expressions.map(_.tpe), position)
      Expression.ListConstruction(expressions, ListType(elementType), position).compiled

    case ObjectMapNode(structName, entryNodes, position) =>
      implicit val pos: Position = position
      registry.resolveType(structName).flatMap {
        case structType: StructType =>
          val entries = entryNodes.zip(expressions).map { case (ObjectEntryNode(name, _, _), expression) => (name, expression) }
          InstantiationTransformation.transformMapStyleInstantiation(structType.definition, entries).map(addJudgmentsFrom)
        case _ => Compilation.fail(StructExpected(structName))
      }

    case ShapeValueNode(propertyNodes, position) =>
      val properties = propertyNodes.zip(expressions).map { case (propertyNode, value) => Expression.ShapeProperty(propertyNode.name, value) }
      Expression.ShapeValue(properties, position).compiled

    // Xary operations.
    case ConjunctionNode(_, position) =>
      transformBooleanOperation(XaryOperator.Conjunction, expressions, position)

    case DisjunctionNode(_, position) =>
      transformBooleanOperation(XaryOperator.Disjunction, expressions, position)

    case ConcatenationNode(_, position) =>
      Expression.XaryOperation(XaryOperator.Concatenation, expressions, BasicType.String, position).compiled

    // Xary function calls.
    case SimpleCallNode(name, _, position) =>
      scopeContext.currentScope.resolve(name)(position).flatMap {
        case mf: MultiFunctionDefinition => FunctionTyping.multiFunctionCall(mf, expressions, node.position).map(addJudgmentsFrom)
        case variable: TypedVariable => transformValueCall(Expression.VariableAccess(variable, position), expressions, position).compiled
      }

    case FixedFunctionCallNode(name, typeExpressions, _, position) =>
      // TODO: Implement later...
      /* for {
        types <- typeExpressions.map(TypeExpressionEvaluator.evaluate).simultaneous
        function <- registry.resolveExactFunction(name, types)(position)
        instance <- function.instantiate(ProductType(types))
      } yield Expression.Call(instance, expressions, position) */
      ???

    case node@DynamicCallNode(resultType, _, position) =>
      (
        // The first argument to the dynamic call must be a constant function name.
        expressions.headOption match {
          case Some(Expression.Literal(name: String, BasicType.String, _)) => name.compiled
          case _ => Compilation.fail(DynamicFunctionNameExpected(node))
        },
        TypeExpressionEvaluator.evaluate(resultType),
      ).simultaneous.map { case (name, resultType) =>
        Expression.Call(CallTarget.Dynamic(name), expressions.tail, resultType, position)
      }
  }

  private def transformBooleanOperation(operator: XaryOperator, expressions: Vector[Expression], position: Position): Compilation[Expression.XaryOperation] = {
    typingJudgments = typingJudgments ++ expressions.map(e => TypingJudgment.Subtypes(e.tpe, BasicType.Boolean, e.position))
    Expression.XaryOperation(operator, expressions, BasicType.Boolean, position).compiled
  }

  override def visitMap(node: MapNode)(kvs: Vector[(Expression, Expression)]): Compilation[Expression] = {
    val entries = kvs.map(Expression.MapEntry.tupled)

    val keyType = new InferenceVariable
    val valueType = new InferenceVariable

    typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(keyType, entries.map(_.key.tpe), node.position)
    typingJudgments = typingJudgments :+ TypingJudgment.LeastUpperBound(valueType, entries.map(_.value.tpe), node.position)

    Expression.MapConstruction(entries, MapType(keyType, valueType), node.position).compiled
  }

  override def visitCall(node: CallNode)(target: Expression, arguments: Vector[Expression]): Compilation[Expression] = {
    transformValueCall(target, arguments, node.position).compiled
  }

  private def transformValueCall(target: Expression, arguments: Vector[Expression], position: Position): Expression.Call = {
    // A call target must be a value with a function type.
    val (inputType, outputType) = target.tpe match {
      // If the target's type is defined now, we can take a shortcut, because it's definitely a function.
      case FunctionType(input, output) => (input, output)

      // May or may not be a function type, so we have to make sure that the type is even a function. The Assign
      // judgment ensures that the target's type is even a function type. For now, we don't want to infer the type of
      // the target based on the provided arguments, so we're relying on one-way type inference.
      case _ =>
        val inputType = new InferenceVariable
        val outputType = new InferenceVariable

        typingJudgments = typingJudgments :+ TypingJudgment.Assign(FunctionType(inputType, outputType), target.tpe, target.position)

        (inputType, outputType)
    }

    typingJudgments = typingJudgments :+ TypingJudgment.Subtypes(ProductType(arguments.map(_.tpe)), inputType, target.position)

    Expression.Call(CallTarget.Value(target), arguments, outputType, position)
  }

  override def visitIteration(node: ForNode)(
    extractorTuples: Vector[(String, Expression)], visitBody: () => Compilation[Expression],
  ): Compilation[Expression] = {
    // Before we visit the body, we have to push a new scope and later, once extractors have been evaluated, also
    // a new loop context.
    scopeContext.openScope()

    def transformExtractor(variableName: String, collection: Expression, position: Position): Compilation[Expression.Extractor] = {
      val elementType = new InferenceVariable
      typingJudgments = typingJudgments :+ TypingJudgment.ElementType(elementType, collection.tpe, position)

      val localVariable = LocalVariable(variableName, elementType, isMutable = false)
      scopeContext.currentScope.register(localVariable)(position).map(_ => Expression.Extractor(localVariable, collection))
    }

    for {
      extractors <- extractorTuples.zip(node.extractors.map(_.position)).map {
        case ((variableName, collection), position) => transformExtractor(variableName, collection, position)
      }.simultaneous
      body <- visitBody()
      _ = scopeContext.closeScope() // We have to close the scope that we opened for the extractors.
      tpe = inferLoopType(body, node.position)
    } yield Expression.ForLoop(extractors, body, tpe, node.position)
  }

  private def inferLoopType(body: Expression, position: Position): InferenceVariable = {
    val resultType = new InferenceVariable
    // The Equals judgment is chosen deliberately so that we can infer the body's type via a potentially explicitly
    // specified result type.
    // For example:
    //    let things = [%{ x: 5 }, %{ x: -2 }, %{ x: 12 }]
    //    let functions: %{ n: Int } => Int = for (v <- things) { v2 => v2.n * v.x }
    // TODO: This also needs to work for map functions...
    // TODO: Would Scala infer this with a for-yield and a map?
    typingJudgments = typingJudgments :+ TypingJudgment.Equals(resultType, ListType(body.tpe), position)
    resultType
  }

  override def before: PartialFunction[TopLevelExprNode, Verification] = {
    case ExprNode.BlockNode(_, _) =>
      scopeContext.openScope()
      Verification.succeed

    case ExprNode.WhileNode(_, _, _) =>
      // A while loop needs to open its own scope in case there is exactly one variable declaration as the loop body,
      // which wouldn't get scoped by the block.
      scopeContext.openScope()
      Verification.succeed

    case ExprNode.AnonymousFunctionNode(parameterNodes, _, _) =>
      scopeContext.openScope()
      parameterNodes.map {
        case AnonymousFunctionParameterNode(name, typeNode, position) =>
          typeNode
            .map(TypeExpressionEvaluator.evaluate).toCompiledOption
            .map(_.getOrElse(new InferenceVariable))
            .flatMap(tpe => scopeContext.currentScope.register(LocalVariable(name, tpe, isMutable = false))(position))
      }.simultaneous.verification
  }

  def addJudgmentsFrom[A]: ((A, Vector[TypingJudgment])) => A = {
    case (value, judgments) =>
      typingJudgments = typingJudgments ++ judgments
      value
  }

}

object InferringExpressionTransformationVisitor {

  case class UnsafeInteger(node: ExprNode.IntLiteralNode) extends Error(node) {
    override def message: String = s"The integer literal ${node.value} is outside the safe run-time range of" +
      s" ${BasicType.Int.minSafeInteger} and ${BasicType.Int.maxSafeInteger}. The Javascript runtime will not be able" +
      s" to properly store and process integers this large."
  }

  case class DynamicFunctionNameExpected(node: ExprNode.DynamicCallNode) extends Error(node) {
    override def message: String = "Dynamic calls require a string literal as their first argument, which represents the" +
      " name of the function. Since the name must be available at compile-time, it must be a constant."
  }

  case class StructExpected(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The name $name must refer to a struct."
  }

  // TODO: This should be the error message for the Assign judgment created in visitCall.
  case class FunctionExpected(variable: TypedVariable, override val position: Position) extends Error(position) {
    override def message: String = s"The variable ${variable.name} should be a function type, but is actually ${variable.tpe}."
  }

}
