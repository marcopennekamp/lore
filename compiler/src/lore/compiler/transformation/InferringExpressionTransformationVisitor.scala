/*
package lore.compiler.transformation

import lore.compiler.core._
import lore.compiler.feedback.{ExpressionFeedback, Feedback, MultiFunctionFeedback, Reporter, StructFeedback}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.resolution.TypeExpressionEvaluator
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, CondCase, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.transformation.InferringExpressionTransformationVisitor._
import lore.compiler.types._
import scalaz.Id.Id

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
    * The binding scope of the surrounding code, such as a function scope.
    */
  bindingScope: BindingScope,
)(implicit registry: Registry, reporter: Reporter) extends TopLevelExprVisitor[Expression, Id] {

  import ExprNode._
  import TopLevelExprNode._

  val scopeContext = new ScopeContext(bindingScope)
  implicit def currentScope: BindingScope = scopeContext.currentScope
  implicit val typeScopeImplicit: TypeScope = typeScope

  // These typing judgments should contain even trivial judgments that could be resolved now. If for example all types
  // are available here to type an addition operation, we could avoid adding any typing judgments and just type the
  // expression here. The problem is that we'd have to still report typing errors, and in a consistent manner with the
  // way type inference reports errors. The amount of duplicated work required here makes this optimization not worth
  // it.
  implicit val judgmentCollector: JudgmentCollector = new JudgmentCollector
  def typingJudgments: Vector[TypingJudgment] = judgmentCollector.judgments

  override def visitLeaf(node: LeafNode): Expression = node match {
    case VariableNode(namePathNode, position) =>
      AccessTransformation
        .transform(BindingProcessors.accessCoercion(position))(namePathNode)
        .getOrElse(Expression.Hole(BasicType.Nothing, position))

    case RealLiteralNode(value, position) => Expression.Literal(value, BasicType.Real, position)
    case node@IntLiteralNode(value, position) =>
      if (value < BasicType.Int.minSafeInteger || BasicType.Int.maxSafeInteger < value) {
        reporter.error(UnsafeInteger(node))
      }
      Expression.Literal(value, BasicType.Int, position)
    case BoolLiteralNode(value, position) => Expression.Literal(value, BasicType.Boolean, position)
    case StringLiteralNode(value, position) => Expression.Literal(value, BasicType.String, position)

    case FixedFunctionNode(namePathNode, typeExpressions, position) =>
      scopeContext.currentScope.resolveStatic(namePathNode.namePath, namePathNode.position).flatMap {
        case mf: MultiFunctionDefinition =>
          val inputType = TupleType(typeExpressions.map(TypeExpressionEvaluator.evaluate).map(_.getOrElse(BasicType.Nothing)))
          mf.dispatch(
            inputType,
            MultiFunctionFeedback.Dispatch.FixedFunctionEmptyFit(mf, inputType, position),
            min => MultiFunctionFeedback.Dispatch.FixedFunctionAmbiguousCall(mf, inputType, min, position),
          ).map(instance => Expression.FixedFunctionValue(instance, position))

        case _ =>
          reporter.error(ExpressionFeedback.FixedFunction.MultiFunctionExpected(namePathNode.namePath, namePathNode.position))
          None
      }.getOrElse(Expression.Hole(BasicType.Nothing, position))

    case ConstructorNode(namePathNode, typeArgumentNodes, position) =>
      StructTransformation.getConstructor(namePathNode.namePath, Some(typeArgumentNodes), namePathNode.position) match {
        case Some(constructor) => Expression.BindingAccess(constructor, position)
        case None => Expression.Hole(BasicType.Nothing, position)
      }

    case SymbolValueNode(name, position) => Expression.Symbol(name, position)
  }

  override def visitUnary(node: UnaryNode)(expression: Expression): Expression = node match {
    case ReturnNode(_, position) =>
      judgmentCollector.add(TypingJudgment.Subtypes(expression.tpe, expectedType, position))
      Expression.Return(expression, position)

    case VariableDeclarationNode(nameNode, isMutable, maybeTypeExpr, _, position) =>
      // Either infer the type from the value or, if a type has been explicitly declared, check that the value adheres
      // to the type bounds. If the type annotation cannot be compiled, it is effectively treated as non-existing.
      val typeAnnotation = maybeTypeExpr.flatMap(TypeExpressionEvaluator.evaluate)
      val tpe = typeAnnotation match {
        case Some(tpe) =>
          judgmentCollector.add(TypingJudgment.Subtypes(expression.tpe, tpe, expression.position))
          tpe

        case None =>
          val tpe = new InferenceVariable
          judgmentCollector.add(TypingJudgment.Assign(tpe, expression.tpe, expression.position))
          tpe
      }

      val variable = LocalVariable(nameNode.value, tpe, isMutable)
      scopeContext.currentScope.register(variable, nameNode.position)
      Expression.VariableDeclaration(variable, expression, position)

    case NegationNode(_, position) =>
      judgmentCollector.add(TypingJudgment.Subtypes(expression.tpe, BasicType.Real, position))
      Expression.UnaryOperation(UnaryOperator.Negation, expression, expression.tpe, position)

    case LogicalNotNode(_, position) =>
      judgmentCollector.add(TypingJudgment.Subtypes(expression.tpe, BasicType.Boolean, position))
      Expression.UnaryOperation(UnaryOperator.LogicalNot, expression, BasicType.Boolean, position)

    case MemberAccessNode(_, nameNode, _) => AccessTransformation.transformMemberAccess(expression, Vector(nameNode))
  }

  override def visitBinary(node: BinaryNode)(left: Expression, right: Expression): Expression = node match {
    case AssignmentNode(_, _, position) =>
      // Ensure that the value on the right can be assigned to the variable or member on the left.
      judgmentCollector.add(TypingJudgment.Subtypes(right.tpe, left.tpe, position))

      left match {
        case access: Expression.Access => Expression.Assignment(access, right, position)
        case _ =>
          // The left-hand-side expression of an assignment must be a variable or a member. The underlying issue, for
          // example that a variable doesn't exist, will have been reported by now.
          Expression.Hole(TupleType.UnitType, position)
      }

    // Arithmetic operations.
    case AdditionNode(_, _, position) => transformNumericOperation(BinaryOperator.Addition, left, right, position)
    case SubtractionNode(_, _, position) => transformNumericOperation(BinaryOperator.Subtraction, left, right, position)
    case MultiplicationNode(_, _, position) => transformNumericOperation(BinaryOperator.Multiplication, left, right, position)
    case DivisionNode(_, _, position) => transformNumericOperation(BinaryOperator.Division, left, right, position)

    // Boolean operations.
    case EqualsNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Equals, left, right, BasicType.Boolean, position)
    case NotEqualsNode(_, _, position) =>
      Expression.UnaryOperation(
        UnaryOperator.LogicalNot,
        Expression.BinaryOperation(BinaryOperator.Equals, left, right, BasicType.Boolean, position),
        BasicType.Boolean,
        position,
      )
    case LessThanNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThan, left, right, BasicType.Boolean, position)
    case LessThanEqualsNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.LessThanEquals, left, right, BasicType.Boolean, position)

    // Collection operations.
    case AppendNode(_, _, position) =>
      val elementType = new InferenceVariable
      val combinedType = new InferenceVariable

      // The Equals judgment is chosen deliberately, because we want the list's type to be able to be inferred from the
      // combined type (which might in turn be inferred from an explicitly typed variable declaration). This inference
      // is possible due to the bidirectionality of the LUB judgment.
      judgmentCollector.add(
        TypingJudgment.Equals(ListType(elementType), left.tpe, position),
        TypingJudgment.LeastUpperBound(combinedType, Vector(elementType, right.tpe), position),
      )

      Expression.BinaryOperation(BinaryOperator.Append, left, right, ListType(combinedType), position)

    // Loops.
    case WhileNode(_, _, position) =>
      // Close the previously opened scope.
      scopeContext.closeScope()
      val condition = left
      val body = right

      judgmentCollector.add(TypingJudgment.Subtypes(condition.tpe, BasicType.Boolean, position))

      Expression.WhileLoop(condition, body, inferLoopType(body, position), position)
  }

  private def transformNumericOperation(
    operator: BinaryOperator,
    left: Expression,
    right: Expression,
    position: Position,
  ): Expression.BinaryOperation = {
    val resultType = new InferenceVariable
    judgmentCollector.add(
      TypingJudgment.Subtypes(left.tpe, BasicType.Real, position),
      TypingJudgment.Subtypes(right.tpe, BasicType.Real, position),
      TypingJudgment.LeastUpperBound(resultType, Vector(left.tpe, right.tpe), position),
    )
    Expression.BinaryOperation(operator, left, right, resultType, position)
  }

  override def visitTernary(node: TernaryNode)(
    argument1: Expression, argument2: Expression, argument3: Expression,
  ): Expression = node match {
    case IfElseNode(_, _, _, position) =>
      val condition = argument1
      val onTrue = argument2
      val onFalse = argument3
      val resultType = new InferenceVariable

      judgmentCollector.add(
        TypingJudgment.Subtypes(condition.tpe, BasicType.Boolean, position),
        TypingJudgment.LeastUpperBound(resultType, Vector(onTrue.tpe, onFalse.tpe), position),
      )

      Expression.IfElse(condition, onTrue, onFalse, resultType, position)
  }

  override def visitXary(node: XaryNode)(expressions: Vector[Expression]): Expression = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      scopeContext.closeScope()
      Expression.Block(expressions, position)

    // Value constructors.
    case TupleNode(_, position) =>
      Expression.Tuple(expressions, position)

    case ListNode(_, position) =>
      val elementType = new InferenceVariable
      judgmentCollector.add(TypingJudgment.LeastUpperBound(elementType, expressions.map(_.tpe), position))
      Expression.ListConstruction(expressions, ListType(elementType), position)

    case ObjectMapNode(namePathNode, typeArgumentNodes, entryNodes, position) =>
      StructTransformation.getConstructor(namePathNode.namePath, typeArgumentNodes, namePathNode.position) match {
        case Some(constructor) =>
          val entries = entryNodes.zip(expressions).map {
            case (ObjectEntryNode(nameNode, _, _), expression) => nameNode.value -> expression
          }
          val arguments = StructTransformation.entriesToArguments(constructor.structType.schema.definition, entries, position)
          CallTransformation.valueCall(Expression.BindingAccess(constructor, namePathNode.position), arguments, position)

        case None => Expression.Hole(BasicType.Nothing, position)
      }

    case ShapeValueNode(propertyNodes, position) =>
      val properties = propertyNodes.zip(expressions).map { case (propertyNode, value) => Expression.ShapeProperty(propertyNode.name, value) }
      Expression.ShapeValue(properties, position)

    // Xary operations.
    case ConjunctionNode(_, position) =>
      transformBooleanOperation(XaryOperator.Conjunction, expressions, position)

    case DisjunctionNode(_, position) =>
      transformBooleanOperation(XaryOperator.Disjunction, expressions, position)

    case ConcatenationNode(_, position) =>
      Expression.XaryOperation(XaryOperator.Concatenation, expressions, BasicType.String, position)

    // Xary function calls.
    case SimpleCallNode(namePathNode, _, position) =>
      def handleValueCall(target: Expression): Expression.Call = {
        CallTransformation.valueCall(target, expressions, position)
      }

      def handleSingleBinding(binding: Binding): Option[Expression.Call] = {
        binding match {
          case mf: MultiFunctionDefinition => Some(FunctionTyping.multiFunctionCall(mf, expressions, position))
          case structBinding: StructConstructorBinding => handleSingleBinding(StructTransformation.getConstructor(structBinding, namePathNode.position))
          case structObject: StructObjectBinding =>
            reporter.error(StructFeedback.Object.NoConstructor(structObject.name, namePathNode.position))
            None
          case binding: TypedBinding => Some(handleValueCall(Expression.BindingAccess(binding, namePathNode.position)))
        }
      }

      AccessTransformation.transform(
        handleSingleBinding,
        BindingProcessors.accessCoercion(namePathNode.position),
        expression => Some(handleValueCall(expression)),
      )(namePathNode).getOrElse(Expression.Hole(BasicType.Nothing, position))

    case DynamicCallNode(nameLiteral, resultTypeNode, _, position) =>
      val resultType = TypeExpressionEvaluator.evaluate(resultTypeNode).getOrElse(BasicType.Nothing)
      Expression.Call(CallTarget.Dynamic(nameLiteral.value), expressions, resultType, position)
  }

  private def transformBooleanOperation(operator: XaryOperator, expressions: Vector[Expression], position: Position): Expression.XaryOperation = {
    judgmentCollector.add(expressions.map(e => TypingJudgment.Subtypes(e.tpe, BasicType.Boolean, e.position)))
    Expression.XaryOperation(operator, expressions, BasicType.Boolean, position)
  }

  override def visitAnonymousFunction(node: AnonymousFunctionNode)(visitBody: () => Expression): Expression = {
    scopeContext.openScope()

    val parameters = node.parameters.map {
      case AnonymousFunctionParameterNode(nameNode, typeNode, position) =>
        // If the type annotation isn't specified or cannot be compiled, we default to an inference variable.
        val tpe = typeNode
          .flatMap(TypeExpressionEvaluator.evaluate)
          .getOrElse(new InferenceVariable)
        val variable = LocalVariable(nameNode.value, tpe, isMutable = false)
        scopeContext.currentScope.register(variable, nameNode.position)
        Expression.AnonymousFunctionParameter(variable.name, variable.tpe, position)
    }

    val body = visitBody()
    scopeContext.closeScope()

    Expression.AnonymousFunction(parameters, body, node.position)
  }

  override def visitMap(node: MapNode)(kvs: Vector[(Expression, Expression)]): Expression = {
    val entries = kvs.map(Expression.MapEntry.tupled)

    val keyType = new InferenceVariable
    val valueType = new InferenceVariable

    judgmentCollector.add(
      TypingJudgment.LeastUpperBound(keyType, entries.map(_.key.tpe), node.position),
      TypingJudgment.LeastUpperBound(valueType, entries.map(_.value.tpe), node.position),
    )

    Expression.MapConstruction(entries, MapType(keyType, valueType), node.position)
  }

  override def visitCall(node: CallNode)(target: Expression, arguments: Vector[Expression]): Expression = {
    CallTransformation.valueCall(target, arguments, node.position)
  }

  override def visitCond(node: CondNode)(rawCases: Vector[(Expression, Expression)]): Expression = {
    val cases = rawCases.map(CondCase.tupled)
    val isTotal = cases.exists(_.isTotalCase)
    val resultType = new InferenceVariable

    judgmentCollector.add(
      cases.map(_.condition).map(condition => TypingJudgment.Subtypes(condition.tpe, BasicType.Boolean, condition.position))
    )

    // If there is no `true` case, we have to assume that the `cond` won't lead to a value in all instances. Hence, we
    // have to assume that Unit may be a result type.
    val bodyTypes = cases.map(_.body.tpe) ++ (if (!isTotal) Vector(TupleType.UnitType) else Vector.empty)
    judgmentCollector.add(TypingJudgment.LeastUpperBound(resultType, bodyTypes, node.position))

    Expression.Cond(cases, resultType, node.position)
  }

  override def visitIteration(node: ForNode)(
    extractorTuples: Vector[(String, Expression)],
    visitBody: () => Expression,
  ): Expression = {
    // Before we visit the body, we have to push a new scope and later, once extractors have been evaluated, also
    // a new loop context.
    scopeContext.openScope()

    def transformExtractor(variableName: String, collection: Expression, position: Position) = {
      val elementType = new InferenceVariable
      judgmentCollector.add(TypingJudgment.ElementType(elementType, collection.tpe, position))

      val variable = LocalVariable(variableName, elementType, isMutable = false)
      scopeContext.currentScope.register(variable, position)
      Expression.Extractor(variable, collection)
    }

    val extractors = extractorTuples.zip(node.extractors.map(_.nameNode.position)).map {
      case ((variableName, collection), position) => transformExtractor(variableName, collection, position)
    }
    val body = visitBody()
    val tpe = inferLoopType(body, node.position)

    // We have to close the scope that we opened for the extractors.
    scopeContext.closeScope()

    Expression.ForLoop(extractors, body, tpe, node.position)
  }

  private def inferLoopType(body: Expression, position: Position): InferenceVariable = {
    val resultType = new InferenceVariable
    // The Equals judgment is chosen deliberately so that we can infer the body's type via a potentially explicitly
    // specified result type.
    // For example:
    //    let things = [%{ x: 5 }, %{ x: -2 }, %{ x: 12 }]
    //    let functions: %{ n: Int } => Int = for (v <- things) { v2 => v2.n * v.x }
    judgmentCollector.add(TypingJudgment.Equals(resultType, ListType(body.tpe), position))
    resultType
  }

  override def before: PartialFunction[TopLevelExprNode, Unit] = {
    case ExprNode.BlockNode(_, _) =>
      scopeContext.openScope()

    case ExprNode.WhileNode(_, _, _) =>
      // A while loop needs to open its own scope in case there is exactly one variable declaration as the loop body,
      // which wouldn't get scoped by the block.
      scopeContext.openScope()
  }

}

object InferringExpressionTransformationVisitor {

  case class UnsafeInteger(node: ExprNode.IntLiteralNode) extends Feedback.Error(node) {
    override def message: String = s"The integer literal ${node.value} is outside the safe run-time range of" +
      s" ${BasicType.Int.minSafeInteger} and ${BasicType.Int.maxSafeInteger}. The Javascript runtime will not be able" +
      s" to properly store and process integers this large."
  }

}
*/
