package lore.compiler.transformation

import lore.compiler.core._
import lore.compiler.feedback.{ExpressionFeedback, MultiFunctionFeedback, Reporter}
import lore.compiler.resolution.TypeResolver
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings._
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.semantics.expressions.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.ExprNode._
import lore.compiler.syntax.TopLevelExprNode._
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.types._
import lore.compiler.typing.MultiReferenceTyping
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Id.Id

class ExpressionTransformationVisitor(
  /**
    * The type scope of the surrounding code, such as a function's type scope.
    */
  typeScope: TypeScope,

  /**
    * The term scope of the surrounding code, such as a function's term scope.
    */
  termScope: TermScope,
)(implicit registry: Registry, reporter: Reporter) extends TopLevelExprVisitor[UntypedExpression, Id] {

  val scopeContext = new ScopeContext(termScope)
  implicit def currentScope: TermScope = scopeContext.currentScope
  implicit val typeScopeImplicit: TypeScope = typeScope

  override def visitLeaf(node: LeafNode): UntypedExpression = node match {
    case VariableNode(namePathNode, position) =>
      AccessTransformation.transform(namePathNode).getOrElse(UntypedHole(BasicType.Nothing, position))

    case node@IntLiteralNode(value, position) =>
      if (value < BasicType.Int.minimum || BasicType.Int.maximum < value) {
        reporter.error(ExpressionFeedback.UnsafeInteger(node))
      }
      UntypedIntValue(value, position)

    case RealLiteralNode(value, position) => UntypedRealValue(value, position)
    case BoolLiteralNode(value, position) => UntypedBooleanValue(value, position)
    case StringLiteralNode(value, position) => UntypedStringValue(value, position)
    case SymbolLiteralNode(name, position) => UntypedSymbolValue(name, position)

    case FixedFunctionNode(namePathNode, typeExpressions, position) =>
      val inputType = TupleType(typeExpressions.map(TypeResolver.resolve).map(_.getOrElse(BasicType.Nothing)))

      def attemptDispatch(mf: MultiFunctionDefinition)(implicit reporter: Reporter) = {
        mf.dispatch(
          inputType,
          MultiFunctionFeedback.FixedFunction.EmptyFit(mf, inputType, position),
          min => MultiFunctionFeedback.FixedFunction.AmbiguousCall(mf, inputType, min, position),
        ).map(instance => UntypedFixedFunctionValue(instance, position))
      }

      scopeContext.currentScope.resolveStatic(namePathNode.namePath, namePathNode.position).flatMap {
        case mf: MultiFunctionDefinition => attemptDispatch(mf)

        case AmbiguousMultiFunction(mfs) =>
          MultiReferenceTyping.disambiguate(mfs, position) {
            case (mf, candidateReporter) => attemptDispatch(mf)(candidateReporter)
          }

        case _ =>
          reporter.error(
            ExpressionFeedback.FixedFunction.MultiFunctionExpected(namePathNode.namePath, namePathNode.position)
          )
          None
      }.getOrElse(UntypedHole(BasicType.Nothing, position))

    case ConstructorNode(namePathNode, typeArgumentNodes, position) =>
      StructTransformation.getConstructorBinding(namePathNode.namePath, namePathNode.position) match {
        case Some(binding) => StructTransformation.getConstructorValue(binding, typeArgumentNodes, namePathNode.position)
        case None => UntypedHole(BasicType.Nothing, position)
      }
  }

  override def visitUnary(node: UnaryNode)(expression: UntypedExpression): UntypedExpression = node match {
    case ReturnNode(_, position) => UntypedReturn(expression, position)

    case VariableDeclarationNode(nameNode, isMutable, maybeTypeExpr, _, position) =>
      // If the type annotation cannot be compiled, it is effectively treated as non-existing.
      val typeAnnotation = maybeTypeExpr.flatMap(TypeResolver.resolve)
      val variable = UntypedLocalVariable(nameNode.value, isMutable)
      scopeContext.currentScope.register(variable, nameNode.position)
      UntypedVariableDeclaration(variable, expression, typeAnnotation, position)

    case NegationNode(_, position) => UntypedUnaryOperation(UnaryOperator.Negation, expression, position)
    case LogicalNotNode(_, position) => UntypedUnaryOperation(UnaryOperator.LogicalNot, expression, position)

    case MemberAccessNode(_, nameNode, _) =>
      AccessTransformation.transformMemberAccess(expression, Vector(nameNode))

    case AscriptionNode(_, expectedTypeNode, position) =>
      val expectedType = TypeResolver.resolve(expectedTypeNode).getOrElse(BasicType.Any)
      UntypedTypeAscription(expression, expectedType, position)
  }

  override def visitBinary(node: BinaryNode)(
    left: UntypedExpression,
    right: UntypedExpression,
  ): UntypedExpression = node match {
    case AssignmentNode(_, _, position) =>
      left match {
        case access: UntypedAccess => UntypedAssignment(access, right, position)
        case _ =>
          // The left-hand-side expression of an assignment must be a variable or a member. The underlying issue, for
          // example that a variable doesn't exist, will have been reported by now.
          UntypedHole(TupleType.UnitType, position)
      }

    // Arithmetic operations.
    case AdditionNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.Addition, left, right, position)

    case SubtractionNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.Subtraction, left, right, position)

    case MultiplicationNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.Multiplication, left, right, position)

    case DivisionNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.Division, left, right, position)

    // Boolean operations.
    case EqualsNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.Equals, left, right, position)

    case NotEqualsNode(_, _, position) =>
      UntypedUnaryOperation(
        UnaryOperator.LogicalNot,
        UntypedBinaryOperation(BinaryOperator.Equals, left, right, position),
        position,
      )

    case LessThanNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.LessThan, left, right, position)

    case LessThanEqualsNode(_, _, position) =>
      UntypedBinaryOperation(BinaryOperator.LessThanEquals, left, right, position)

    // Collection operations.
    case AppendNode(_, _, position) => UntypedBinaryOperation(BinaryOperator.Append, left, right, position)

    // Loops.
    case WhileNode(_, _, position) =>
      // Close the previously opened scope.
      scopeContext.closeScope()

      UntypedWhileLoop(left, right, position)
  }

  override def visitTernary(node: TernaryNode)(
    argument1: UntypedExpression,
    argument2: UntypedExpression,
    argument3: UntypedExpression,
  ): UntypedExpression = node match {
    case IfElseNode(_, _, onFalse, position) =>
      val cases = Vector(
        UntypedCondCase(argument1, argument2),
        UntypedCondCase(UntypedBooleanValue(value = true, onFalse.map(_.position).getOrElse(position)), argument3)
      )
      UntypedCond(cases, position)
  }

  override def visitXary(node: XaryNode)(expressions: Vector[UntypedExpression]): UntypedExpression = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      scopeContext.closeScope()
      UntypedBlock(expressions.withDefault(UntypedTupleValue(Vector.empty, position)), position)

    // Value constructors.
    case TupleNode(_, position) => UntypedTupleValue(expressions, position)
    case ListNode(_, position) => UntypedListValue(expressions, position)

    case ObjectMapNode(namePathNode, typeArgumentNodes, entryNodes, position) =>
      StructTransformation.getConstructorBinding(namePathNode.namePath, namePathNode.position) match {
        case Some(binding) =>
          val entries = entryNodes.zip(expressions).map {
            case (ObjectEntryNode(nameNode, _, _), expression) => nameNode.value -> expression
          }
          val arguments = StructTransformation.entriesToArguments(binding.underlyingType.schema, entries, position)
          typeArgumentNodes match {
            case Some(typeArgumentNodes) =>
              val target = StructTransformation.getConstructorValue(binding, typeArgumentNodes, namePathNode.position)
              UntypedValueCall(target, arguments, position)

            case None => UntypedConstructorCall(binding, arguments, position)
          }

        case None => UntypedHole(BasicType.Nothing, position)
      }

    case ShapeValueNode(propertyNodes, position) =>
      val properties = propertyNodes.zip(expressions).map {
        case (propertyNode, value) => UntypedShapeProperty(propertyNode.name, value, propertyNode.position)
      }
      UntypedShapeValue(properties, position)

    // Xary operations.
    case ConjunctionNode(_, position) => UntypedXaryOperation(XaryOperator.Conjunction, expressions, position)
    case DisjunctionNode(_, position) => UntypedXaryOperation(XaryOperator.Disjunction, expressions, position)
    case ConcatenationNode(_, position) => UntypedXaryOperation(XaryOperator.Concatenation, expressions, position)

    // Xary function calls.
    case node: SimpleCallNode => CallTransformation.transformSimpleCall(node, expressions)
    case node: IntrinsicCallNode => CallTransformation.transformIntrinsicCall(node, expressions)
  }

  override def visitLambdaValue(node: LambdaValueNode)(
    visitBody: () => UntypedExpression,
  ): UntypedExpression = {
    scopeContext.openScope()

    val parameters = node.parameters.map {
      case LambdaParameterNode(nameNode, typeNode, position) =>
        val typeAnnotation = typeNode.flatMap(TypeResolver.resolve)
        val variable = UntypedLocalVariable(nameNode.value, isMutable = false)
        scopeContext.currentScope.register(variable, nameNode.position)
        UntypedLambdaParameter(variable, typeAnnotation, position)
    }

    val body = visitBody()
    scopeContext.closeScope()

    UntypedLambdaValue(parameters, body, node.position)
  }

  override def visitMap(node: MapNode)(kvs: Vector[(UntypedExpression, UntypedExpression)]): UntypedExpression = {
    ???
  }

  override def visitCall(node: CallNode)(
    target: UntypedExpression,
    arguments: Vector[UntypedExpression],
  ): UntypedExpression = {
    UntypedValueCall(target, arguments, node.position)
  }

  override def visitCond(node: CondNode)(
    rawCases: Vector[(UntypedExpression, UntypedExpression)],
  ): UntypedExpression = {
    var cases = rawCases.map(UntypedCondCase.tupled)
    if (cases.init.exists(_.isTotalCase)) {
      reporter.error(ExpressionFeedback.InvalidTotalCase(node))
    }

    // If the last case isn't total, we have to add a default case `true => ()`.
    if (!cases.lastOption.exists(_.isTotalCase)) {
      cases = cases :+ UntypedCondCase(
        UntypedBooleanValue(value = true, node.position),
        UntypedTupleValue(Vector.empty, node.position),
      )
    }

    UntypedCond(cases, node.position)
  }

  override def visitIteration(node: ForNode)(
    visitExtractors: Vector[() => UntypedExpression],
    visitBody: () => UntypedExpression,
  ): UntypedExpression = {
    scopeContext.openScope()

    def transformExtractor(
      variableName: String,
      visitExtractor: () => UntypedExpression,
      position: Position,
    ) = {
      val collection = visitExtractor()

      // Each extractor declares a new scope, as extractor variables may be redefined from left to right. The
      // extractor's collection must be resolved outside this new scope so that it can use a previous extractor
      // variable or a variable from the outer scope. This enables loops such as `for xs <- xs, xs <- foo(xs)`.
      scopeContext.openScope()
      val variable = UntypedLocalVariable(variableName, isMutable = false)
      scopeContext.currentScope.register(variable, position)
      UntypedExtractor(variable, collection)
    }

    val extractors = visitExtractors.zip(node.extractors).map { case (visitExtractor, extractorNode) =>
      transformExtractor(extractorNode.name, visitExtractor, extractorNode.nameNode.position)
    }
    val body = visitBody()

    // We have to close the scopes that were opened for the extractors.
    for (_ <- visitExtractors.indices) {
      scopeContext.closeScope()
    }

    UntypedForLoop(extractors, body, node.position)
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
