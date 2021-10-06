package lore.compiler.transformation2

import lore.compiler.core._
import lore.compiler.feedback.{ExpressionFeedback, MultiFunctionFeedback, Reporter, StructFeedback}
import lore.compiler.inference.InferenceVariable
import lore.compiler.resolution.TypeExpressionEvaluator
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, CondCase, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.scopes._
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.transformation._
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Id.Id

class ExpressionTransformationVisitor(
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

  override def visitLeaf(node: LeafNode): Expression = node match {
    case VariableNode(namePathNode, position) =>
      AccessTransformation
        .transform(BindingProcessors.accessCoercion(position))(namePathNode)
        .getOrElse(Expression.Hole(BasicType.Nothing, position))

    case RealLiteralNode(value, position) => Expression.Literal(value, BasicType.Real, position)
    case node@IntLiteralNode(value, position) =>
      if (value < BasicType.Int.minSafeInteger || BasicType.Int.maxSafeInteger < value) {
        reporter.error(ExpressionFeedback.UnsafeInteger(node))
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
      StructTransformation.getConstructorBinding(namePathNode.namePath, namePathNode.position) match {
        case Some(binding) => StructTransformation.getConstructorValue(binding, typeArgumentNodes, namePathNode.position)
        case None => Expression.Hole(BasicType.Nothing, position)
      }

    case SymbolValueNode(name, position) => Expression.Symbol(name, position)
  }

  override def visitUnary(node: UnaryNode)(expression: Expression): Expression = node match {
    case ReturnNode(_, position) => Expression.Return(expression, position)

    case VariableDeclarationNode(nameNode, isMutable, maybeTypeExpr, _, position) =>
      // If the type annotation cannot be compiled, it is effectively treated as non-existing.
      val typeAnnotation = maybeTypeExpr.flatMap(TypeExpressionEvaluator.evaluate)
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
    case IfElseNode(_, _, _, position) =>
      val cases = Vector(
        CondCase(argument1, argument2),
        CondCase(Expression.Literal(true, BasicType.Boolean, Position.internal), argument3)
      )
      Expression.Cond(cases, position)
  }

  override def visitXary(node: XaryNode)(expressions: Vector[Expression]): Expression = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      scopeContext.closeScope()
      Expression.Block(expressions.withDefault(Expression.Tuple(Vector.empty, position)), position)

    // Value constructors.
    case TupleNode(_, position) =>
      Expression.Tuple(expressions, position)

    case ListNode(_, position) =>
      Expression.ListConstruction(expressions, position)

    case ObjectMapNode(namePathNode, typeArgumentNodes, entryNodes, position) =>
      StructTransformation.getConstructorBinding(namePathNode.namePath, namePathNode.position) match {
        case Some(binding) =>
          val entries = entryNodes.zip(expressions).map {
            case (ObjectEntryNode(nameNode, _, _), expression) => nameNode.value -> expression
          }
          val arguments = StructTransformation.entriesToArguments(binding.underlyingType.schema.definition, entries, position)
          typeArgumentNodes match {
            case Some(typeArgumentNodes) =>
              val target = StructTransformation.getConstructorValue(binding, typeArgumentNodes, namePathNode.position)
              CallTransformation.valueCall(target, arguments, position)

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
        CallTransformation.valueCall(target, expressions, position)
      }

      def handleSingleBinding(binding: Binding): Option[Expression.Call] = {
        binding match {
          case mf: MultiFunctionDefinition => Some(Expression.Call(CallTarget.MultiFunction(mf), expressions, new InferenceVariable, position))
          case structBinding: StructConstructorBinding => Some(Expression.Call(CallTarget.Constructor(structBinding), expressions, new InferenceVariable, position))
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
    Expression.MapConstruction(kvs.map(Expression.MapEntry.tupled), node.position)
  }

  override def visitCall(node: CallNode)(target: Expression, arguments: Vector[Expression]): Expression = {
    CallTransformation.valueCall(target, arguments, node.position)
  }

  override def visitCond(node: CondNode)(rawCases: Vector[(Expression, Expression)]): Expression = {
    val cases = rawCases.map(CondCase.tupled)
    Expression.Cond(cases, node.position)
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
      val variable = LocalVariable(variableName, elementType, isMutable = false)
      scopeContext.currentScope.register(variable, position)
      Expression.Extractor(variable, collection)
    }

    val extractors = extractorTuples.zip(node.extractors.map(_.nameNode.position)).map {
      case ((variableName, collection), position) => transformExtractor(variableName, collection, position)
    }
    val body = visitBody()

    // We have to close the scope that we opened for the extractors.
    scopeContext.closeScope()

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
