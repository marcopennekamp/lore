package lore.compiler.transformation

import lore.compiler.core._
import lore.compiler.feedback.{ExpressionFeedback, MultiFunctionFeedback, Reporter, StructFeedback}
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.resolution.TypeResolver
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings._
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, CondCase, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable
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
)(implicit registry: Registry, reporter: Reporter) extends TopLevelExprVisitor[Expression, Id] {

  import ExprNode._
  import TopLevelExprNode._

  val scopeContext = new ScopeContext(termScope)
  implicit def currentScope: TermScope = scopeContext.currentScope
  implicit val typeScopeImplicit: TypeScope = typeScope

  override def visitLeaf(node: LeafNode): Expression = node match {
    case VariableNode(namePathNode, position) =>
      AccessTransformation
        .transform(BindingProcessors.accessCoercion(position))(namePathNode)
        .getOrElse(Expression.Hole(BasicType.Nothing, position))

    case node@IntLiteralNode(value, position) =>
      if (value < BasicType.Int.minimum || BasicType.Int.maximum < value) {
        reporter.error(ExpressionFeedback.UnsafeInteger(node))
      }
      Expression.Literal.integer(value, position)
    case RealLiteralNode(value, position) => Expression.Literal.real(value, position)
    case BoolLiteralNode(value, position) => Expression.Literal.boolean(value, position)
    case StringLiteralNode(value, position) => Expression.Literal.string(value, position)
    case SymbolLiteralNode(name, position) => Expression.Literal.symbol(name, position)

    case FixedFunctionNode(namePathNode, typeExpressions, position) =>
      scopeContext.currentScope.resolveStatic(namePathNode.namePath, namePathNode.position).flatMap {
        case mf: MultiFunctionDefinition =>
          val inputType = TupleType(typeExpressions.map(TypeResolver.resolve).map(_.getOrElse(BasicType.Nothing)))
          mf.dispatch(
            inputType,
            MultiFunctionFeedback.FixedFunction.EmptyFit(mf, inputType, position),
            min => MultiFunctionFeedback.FixedFunction.AmbiguousCall(mf, inputType, min, position),
          ).map(instance => Expression.FixedFunctionValue(instance, position))

        case _ =>
          reporter.error(ExpressionFeedback.FixedFunction.MultiFunctionExpected(namePathNode.namePath, namePathNode.position))
          None
      }.getOrElse(Expression.Hole(BasicType.Nothing, position))

    case ConstructorNode(namePathNode, typeArgumentNodes, position) =>
      StructTransformation.getConstructorBinding(namePathNode.namePath, namePathNode.position) match {
        case Some(binding) => StructTransformation.getConstructorValue(binding, typeArgumentNodes, namePathNode.position)
        case None => Expression.Hole(BasicType.Nothing, position)
      }
  }

  override def visitUnary(node: UnaryNode)(expression: Expression): Expression = node match {
    case ReturnNode(_, position) => Expression.Return(expression, position)

    case VariableDeclarationNode(nameNode, isMutable, maybeTypeExpr, _, position) =>
      // If the type annotation cannot be compiled, it is effectively treated as non-existing.
      val typeAnnotation = maybeTypeExpr.flatMap(TypeResolver.resolve)
      val tpe = typeAnnotation match {
        case Some(tpe) => tpe
        case None => new InferenceVariable
      }

      val variable = LocalVariable(nameNode.value, tpe, isMutable)
      scopeContext.currentScope.register(variable, nameNode.position)
      Expression.VariableDeclaration(variable, expression, typeAnnotation, position)

    case NegationNode(_, position) => Expression.UnaryOperation(UnaryOperator.Negation, expression, expression.tpe, position)
    case LogicalNotNode(_, position) => Expression.UnaryOperation(UnaryOperator.LogicalNot, expression, BasicType.Boolean, position)

    case MemberAccessNode(_, nameNode, _) => AccessTransformation.transformMemberAccess(expression, Vector(nameNode))

    case AscriptionNode(_, expectedTypeNode, position) =>
      val expectedType = TypeResolver.resolve(expectedTypeNode).getOrElse(BasicType.Any)
      Expression.Ascription(expression, expectedType, position)
  }

  override def visitBinary(node: BinaryNode)(left: Expression, right: Expression): Expression = node match {
    case AssignmentNode(_, _, position) =>
      left match {
        case access: Expression.Access => Expression.Assignment(access, right, position)
        case _ =>
          // The left-hand-side expression of an assignment must be a variable or a member. The underlying issue, for
          // example that a variable doesn't exist, will have been reported by now.
          Expression.Hole(TupleType.UnitType, position)
      }

    // Arithmetic operations.
    case AdditionNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Addition, left, right, new InferenceVariable, position)
    case SubtractionNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Subtraction, left, right, new InferenceVariable, position)
    case MultiplicationNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Multiplication, left, right, new InferenceVariable, position)
    case DivisionNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Division, left, right, new InferenceVariable, position)

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
    case AppendNode(_, _, position) => Expression.BinaryOperation(BinaryOperator.Append, left, right, ListType(new InferenceVariable), position)

    // Loops.
    case WhileNode(_, _, position) =>
      // Close the previously opened scope.
      scopeContext.closeScope()

      Expression.WhileLoop(left, right, position)
  }

  override def visitTernary(node: TernaryNode)(
    argument1: Expression, argument2: Expression, argument3: Expression,
  ): Expression = node match {
    case IfElseNode(_, _, onFalse, position) =>
      val cases = Vector(
        CondCase(argument1, argument2),
        CondCase(Expression.Literal.boolean(value = true, onFalse.map(_.position).getOrElse(position)), argument3)
      )
      Expression.Cond(cases, position)
  }

  override def visitXary(node: XaryNode)(expressions: Vector[Expression]): Expression = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      scopeContext.closeScope()
      Expression.Block(expressions.withDefault(Expression.TupleValue(Vector.empty, position)), new InferenceVariable, position)

    // Value constructors.
    case TupleNode(_, position) =>
      Expression.TupleValue(expressions, position)

    case ListNode(_, position) =>
      Expression.ListValue(expressions, position)

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
              Expression.Call(CallTarget.Value(target), arguments, new InferenceVariable, position)

            case None =>
              val target = CallTarget.Constructor(binding)
              Expression.Call(target, arguments, new InferenceVariable, position)
          }

        case None => Expression.Hole(BasicType.Nothing, position)
      }

    case ShapeValueNode(propertyNodes, position) =>
      val properties = propertyNodes.zip(expressions).map { case (propertyNode, value) => Expression.ShapeProperty(propertyNode.name, value) }
      Expression.ShapeValue(properties, position)

    // Xary operations.
    case ConjunctionNode(_, position) => Expression.XaryOperation(XaryOperator.Conjunction, expressions, BasicType.Boolean, position)
    case DisjunctionNode(_, position) => Expression.XaryOperation(XaryOperator.Disjunction, expressions, BasicType.Boolean, position)
    case ConcatenationNode(_, position) => Expression.XaryOperation(XaryOperator.Concatenation, expressions, BasicType.String, position)

    // Xary function calls.
    case SimpleCallNode(namePathNode, _, position) =>
      def handleValueCall(target: Expression): Expression.Call = {
        Expression.Call(CallTarget.Value(target), expressions, new InferenceVariable, position)
      }

      def handleSingleAccess(binding: TermBinding): Option[Expression.Call] = {
        binding match {
          case mf: MultiFunctionDefinition =>
            Some(Expression.Call(CallTarget.MultiFunction(mf), expressions, new InferenceVariable, position))

          case AmbiguousMultiFunction(multiReference) => ??? // TODO (multi-import): Implement.

          case structBinding: StructConstructorBinding =>
            Some(Expression.Call(CallTarget.Constructor(structBinding), expressions, new InferenceVariable, position))

          case structObject: StructObjectBinding =>
            reporter.error(StructFeedback.Object.NoConstructor(structObject.name, namePathNode.position))
            None

          case binding: TypedTermBinding =>
            Some(handleValueCall(Expression.BindingAccess(binding, namePathNode.position)))
        }
      }

      AccessTransformation.transform(
        handleSingleAccess,
        BindingProcessors.accessCoercion(namePathNode.position),
        expression => Some(handleValueCall(expression)),
      )(namePathNode).getOrElse(Expression.Hole(BasicType.Nothing, position))

    case node@IntrinsicCallNode(nameLiteral, resultTypeNode, _, position) =>
      val name = nameLiteral.value
      val resultType = TypeResolver.resolve(resultTypeNode).getOrElse(BasicType.Nothing)

      // We have to check the existence and arity of the invoked intrinsic before we can create the Call expression.
      PoemIntrinsic.intrinsicsMap.get(name) match {
        case Some(intrinsic) =>
          if (intrinsic.arity != expressions.length) {
            reporter.error(ExpressionFeedback.Intrinsic.IllegalArity(node, intrinsic, expressions.length))
          }
          Expression.Call(CallTarget.Intrinsic(intrinsic), expressions, resultType, position)

        case None =>
          reporter.error(ExpressionFeedback.Intrinsic.NotFound(node, name))
          Expression.Hole(resultType, position)
      }
  }

  override def visitAnonymousFunction(node: AnonymousFunctionNode)(visitBody: () => Expression): Expression = {
    scopeContext.openScope()

    val parameters = node.parameters.map {
      case AnonymousFunctionParameterNode(nameNode, typeNode, position) =>
        // If the type annotation isn't specified or cannot be compiled, we default to an inference variable.
        val tpe = typeNode
          .flatMap(TypeResolver.resolve)
          .getOrElse(new InferenceVariable)
        val variable = LocalVariable(nameNode.value, tpe, isMutable = false)
        scopeContext.currentScope.register(variable, nameNode.position)
        Expression.LambdaParameter(variable.uniqueKey, variable.name, variable.tpe, position)
    }

    val body = visitBody()
    scopeContext.closeScope()

    Expression.LambdaValue(parameters, body, node.position)
  }

  override def visitMap(node: MapNode)(kvs: Vector[(Expression, Expression)]): Expression = {
    Expression.MapConstruction(kvs.map(Expression.MapEntry.tupled), node.position)
  }

  override def visitCall(node: CallNode)(target: Expression, arguments: Vector[Expression]): Expression = {
    Expression.Call(CallTarget.Value(target), arguments, new InferenceVariable, node.position)
  }

  override def visitCond(node: CondNode)(rawCases: Vector[(Expression, Expression)]): Expression = {
    var cases = rawCases.map(CondCase.tupled)
    if (cases.init.exists(_.isTotalCase)) {
      reporter.error(ExpressionFeedback.InvalidTotalCase(node))
    }

    // If the last case isn't total, we have to add a default case `true => ()`.
    if (!cases.lastOption.exists(_.isTotalCase)) {
      cases = cases :+ CondCase(
        Expression.Literal(Expression.Literal.BooleanValue(true), node.position),
        Expression.TupleValue(Vector.empty, node.position),
      )
    }

    Expression.Cond(cases, node.position)
  }

  override def visitIteration(node: ForNode)(
    visitExtractors: Vector[() => Expression],
    visitBody: () => Expression,
  ): Expression = {
    scopeContext.openScope()

    def transformExtractor(variableName: String, visitExtractor: () => Expression, position: Position) = {
      val collection = visitExtractor()

      // Each extractor declares a new scope, as extractor variables may be redefined from left to right. The
      // extractor's collection must be resolved outside this new scope so that it can use a previous extractor
      // variable or a variable from the outer scope. This enables loops such as `for xs <- xs, xs <- foo(xs)`.
      scopeContext.openScope()
      val elementType = new InferenceVariable
      val variable = LocalVariable(variableName, elementType, isMutable = false)
      scopeContext.currentScope.register(variable, position)
      Expression.Extractor(variable, collection)
    }

    val extractors = visitExtractors.zip(node.extractors).map {
      case (visitExtractor, extractorNode) => transformExtractor(extractorNode.name, visitExtractor, extractorNode.nameNode.position)
    }
    val body = visitBody()

    // We have to close the scopes that were opened for the extractors.
    for (_ <- visitExtractors.indices) {
      scopeContext.closeScope()
    }

    Expression.ForLoop(extractors, body, node.position)
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
