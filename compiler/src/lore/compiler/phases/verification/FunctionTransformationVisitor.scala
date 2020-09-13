package lore.compiler.phases.verification

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions._
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.semantics.{LocalVariable, Registry, TypeScope}
import lore.compiler.syntax.visitor.StmtVisitor
import lore.compiler.syntax.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.types._

private[verification] class FunctionTransformationVisitor(
  /**
    * The signature of the function or constructor for which we want to infer types.
    */
  topSignature: FunctionSignature,

  /**
    * The type scope of the function or constructor for which we want to infer types.
    */
  functionTypeScope: TypeScope,

  /**
    * The class that owns the constructor IF the signature represents a constructor.
    */
  classDefinition: Option[StructDefinition],
)(implicit registry: Registry) extends StmtVisitor[Expression] {
  import ExprNode._
  import FunctionTransformationVisitor._
  import StatementTransformation._
  import StmtNode._
  import TopLevelExprNode._

  // TODO: Ensure that loops with a Unit expression body cannot be used as an expression, since Unit loops
  //       are optimized by the transpiler.

  /**
    * The type scope is made implicit so that it can be used throughout the verification process. This allows
    * us to easily handle type variables defined with a function or even those defined with the class of a
    * constructor.
    */
  implicit val typeScope: TypeScope = functionTypeScope

  /**
    * The function verification context used by the visitor to open and close scopes.
    */
  val context = new FunctionTransformationContext(topSignature)

  // TODO: Move more code to StatementVerification.

  override def visitLeaf(node: LeafNode): Compilation[Expression] = node match {
    case VariableNode(name, _) =>
      // TODO: We will also need access to global variables if we introduce those into Lore.
      // TODO: Once we treat functions as values, we will have to make this even more complicated by also
      //       considering function names.
      implicit val position: Position = node.position
      context.currentScope.resolve(name).map(Expression.VariableAccess(_, position))

    case RealLiteralNode(value, position) => Expression.Literal(value, BasicType.Real, position).compiled
    case IntLiteralNode(value, position) => Expression.Literal(value, BasicType.Int, position).compiled
    case BoolLiteralNode(value, position) => Expression.Literal(value, BasicType.Boolean, position).compiled
    case StringLiteralNode(value, position) => Expression.Literal(value, BasicType.String, position).compiled
    case UnitNode(position) => Expression.Tuple(Nil, position).compiled
  }

  override def visitUnary(node: UnaryNode)(expression: Expression): Compilation[Expression] = node match {
    case ReturnNode(_, position) =>
      ExpressionVerification.hasSubtype(expression, topSignature.outputType).map(_ => Expression.Return(expression, position))

    case VariableDeclarationNode(name, isMutable, maybeTypeNode, _, _) =>
      implicit val position: Position = node.position
      for {
        // Either infer the type from the value or, if a type has been explicitly declared, check that the
        // value adheres to the type bounds.
        tpe <- maybeTypeNode.map { typeNode =>
          TypeExpressionEvaluator.evaluate(typeNode).flatMap { tpe =>
            ExpressionVerification.hasSubtype(expression, tpe).map(_ => tpe)
          }
        }.toCompiledOption.map(_.getOrElse(expression.tpe))
        variable = LocalVariable(name, tpe, isMutable)
        _ <- context.currentScope.register(variable)
      } yield Expression.VariableDeclaration(variable, expression, position)

    case NegationNode(_, position) =>
      ExpressionVerification.isNumeric(expression).map(_ => Expression.UnaryOperation(UnaryOperator.Negation, expression, expression.tpe, position))
    case LogicalNotNode(_, position) =>
      ExpressionVerification.isBoolean(expression).map(_ => Expression.UnaryOperation(UnaryOperator.LogicalNot, expression, BasicType.Boolean, position))

    case MemberAccessNode(_, name, _) =>
      implicit val position: Position = node.position
      MemberExplorer.find(name, expression.tpe).map(member => Expression.MemberAccess(expression, member, position))
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
        case memberAccess: Expression.MemberAccess => (memberAccess.tpe, memberAccess.member.isMutable)
      }
      for {
        // Ensure that the variable or property is even mutable.
        _ <- Verification.fromErrors(if (!isMutable) ImmutableAssignment(access) :: Nil else Nil)
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
      StatementTransformation.transformComparison("isLessThanOrEqual", BinaryOperator.LessThan, left, right, position)
    case GreaterThanNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThan", BinaryOperator.LessThan, right, left, position)
    case GreaterThanEqualsNode(_, _, position) =>
      StatementTransformation.transformComparison("isLessThanOrEqual", BinaryOperator.LessThan, right, left, position)

    // Collection operations.
    case AppendNode(_, _, position) =>
      left.tpe match {
        case ListType(elementType) =>
          // The immutable list is constructed from the existing list and another element. The resulting type of the
          // list will be the least upper bound of the two types. If the two types aren't related, we default to sum
          // types, which provide a sensible default for complex list construction.
          // TODO: If we have an append between, say, a type variable A <: Mammal as the list's element type and a
          //       new element type of Human, should we decide the type at run-time (LUB of A and Human) or at
          //       compile-time? I think currently, the type is decided at compile-time, which means that with a
          //       correct implementation of LUBs for type variables, the type will just be Mammal. It could be
          //       narrower at run-time, but that would mean rethinking list types once again. Maybe we should let
          //       it be as is, focus on other aspects of the language, and later come back here once we have more
          //       feedback from actually using Lore (both my own feedback and maybe user feedback).
          val combinedType = LeastUpperBound.leastUpperBound(elementType, right.tpe)
          Expression.BinaryOperation(BinaryOperator.Append, left, right, ListType(combinedType), position).compiled
        // TODO: Implement append for maps?
        case _ => Compilation.fail(ExpressionVerification.IllegallyTypedExpression(left, ListType(BasicType.Any) :: Nil))
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
      // TODO: Warn if the result type is Any?
      ExpressionVerification.isBoolean(condition).map { _ =>
        // TODO: If only one branch supplies a value, return an OPTION of the evaluated type. Of course, we don't
        //       HAVE options just yet. This also needs to become part of the spec before it's implemented, IF we
        //       implement this feature.
        val tpe = LeastUpperBound.leastUpperBound(onTrue.tpe, onFalse.tpe)
        Expression.IfElse(condition, onTrue, onFalse, tpe, position)
      }
  }

  override def visitXary(node: XaryNode)(expressions: List[Expression]): Compilation[Expression] = node match {
    case BlockNode(_, position) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      context.closeScope()
      Expression.Block(expressions, position).compiled

    // Value constructors.
    case TupleNode(_, position) => Expression.Tuple(expressions, position).compiled
    case ListNode(_, position) =>
      // If we type empty lists as [Nothing], we can assign this empty list to any kind of list, which makes
      // coders happy. :) Hence the default value in the fold.
      val elementType = expressions.map(_.tpe).foldLeft(BasicType.Nothing: Type)(LeastUpperBound.leastUpperBound)
      Expression.ListConstruction(expressions, ListType(elementType), position).compiled

    // Xary operations.
    case ConjunctionNode(_, position) => StatementTransformation.transformBooleanOperation(XaryOperator.Conjunction, expressions, position)
    case DisjunctionNode(_, position) => StatementTransformation.transformBooleanOperation(XaryOperator.Disjunction, expressions, position)
    case ConcatenationNode(_, position) =>
      // TODO: We actually have to ensure that the current context has a registered function "toString".
      // TODO: Do we HAVE to hard-code the function name? Maybe this could be specified as a compiler option? Or
      //       am I overthinking this. The idea was, from the start, to make pyramid an "optional" drop-in. So this
      //       feels like one step of coupling the the compiler and pyramid together. We'll have to see.
      for {
        transformedExpressions <- expressions.map { expression =>
          if (expression.tpe == BasicType.String) expression.compiled
          else ExpressionBuilder.multiFunctionCall("toString", List(expression), expression.position)
        }.simultaneous
      } yield Expression.XaryOperation(XaryOperator.Concatenation, transformedExpressions, BasicType.String, position)

    // Function calls.
    case SimpleCallNode(name, qualifier, _, position) =>
      implicit val pos: Position = position
      // A simple call may either be a function or a constructor call. We immediately try to differentiate this based
      // on whether a class type can be found for the function name.
      registry.getStructType(name) match {
        case Some(classType) => StatementTransformation.transformConstructorCall(classType.definition, qualifier, expressions)
        case None =>
          // If we couldn't find a class type for the function, but qualifier is not None, this is not a valid function
          // call and we assume that the type name is wrong. Hence the TypeNotFound error.
          if (qualifier.isDefined) Compilation.fail(Registry.TypeNotFound(name))
          else ExpressionBuilder.multiFunctionCall(name, expressions, position)
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
    case node@ConstructorCallNode(qualifier, isSuper, _, position) =>
      implicit val pos: Position = position
      // Continue with a constructor this.name. We have already verified that only a constructor can contain a
      // continuation, so we can safely get the class definition here as part of the contract of this visitor.
      classDefinition match {
        case None => throw CompilationException("Only constructors can contain continuations.")
        case Some(definition) =>
          for {
            targetDefinition <- if (isSuper) {
              definition.supertypeDefinition match {
                case None => Compilation.fail(MissingSuperConstructor(node))
                case Some(sd) => sd.compiled
              }
            } else definition.compiled
            call <- StatementTransformation.transformConstructorCall(targetDefinition, qualifier, expressions)
          } yield call
      }
  }

  override def visitConstruct(node: ConstructNode)(arguments: List[Expression], withSuper: Option[Expression]): Compilation[Expression] = {
    classDefinition match {
      case None => throw CompilationException("Only constructors can contain continuations.")
      case Some(definition) =>
        ExpressionVerification.adhereToSignature(arguments, definition.constructSignature, node.position).map { _ =>
          Expression.Construct(definition, arguments, withSuper, node.position)
        }
    }
  }

  override def visitMap(node: MapNode)(kvs: List[(Expression, Expression)]): Compilation[Expression] = {
    val entries = kvs.map(Expression.MapEntry.tupled)
    val keyType = entries.map(_.key.tpe).foldLeft(BasicType.Nothing: Type)(LeastUpperBound.leastUpperBound)
    val valueType = entries.map(_.value.tpe).foldLeft(BasicType.Nothing: Type)(LeastUpperBound.leastUpperBound)
    Expression.MapConstruction(entries, MapType(keyType, valueType), node.position).compiled
  }

  override def visitIteration(node: ForNode)(
    extractorTuples: List[(String, Expression)], visitBody: () => Compilation[Expression],
  ): Compilation[Expression] = {
    // TODO: Alternative solution: Add a function visitExtractor which visits the extractor nodes first. Then we can
    //       open the scope and loop context in before, add each extractor to the scope in visitExtractor, and clean
    //       up the scope and list context in this method, visit iteration. (Or we can go back to pattern-matching
    //       up in the verify method again.)
    // TODO: Also consider using a custom Applicator instead.

    // Before we visit the body, we have to push a new scope and later, once extractors have been evaluated, also
    // a new loop context.
    context.openScope()
    val scope = context.currentScope

    def transformExtractor(variableName: String, collection: Expression, position: Position): Compilation[Expression.Extractor] = {
      for {
        elementType <- collection.tpe match {
          case ListType(element) => Compilation.succeed(element)
          case MapType(key, value) => Compilation.succeed(ProductType(List(key, value)))
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

private[verification] object FunctionTransformationVisitor {
  case class ImmutableAssignment(access: Expression.Access) extends Error(access) {
    override def message = s"The variable or member ${access.name} you are trying to assign to is immutable."
  }

  case class EmptyFit(mf: MultiFunctionDefinition, inputType: Type)(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot" +
      s" find a function of that name that would accept the given arguments with the type $inputType."
  }

  case class AmbiguousCall(
    mf: MultiFunctionDefinition, inputType: Type, min: List[FunctionDefinition],
  )(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an ambiguous min-set." +
      s" That is, we are finding TOO MANY functions that would accept the given arguments with the type $inputType." +
      s" These are: ${min.mkString(", ")}."
  }

  case class DynamicFunctionNameExpected()(implicit position: Position) extends Error(position) {
    override def message: String = "Dynamic calls require a string literal as their first argument, which represents the" +
      " name of the function. Since the name must be available at compile-time, it must be a constant."
  }

  case class MissingSuperConstructor(node: TopLevelExprNode.ConstructorCallNode) extends Error(node) {
    override def message: String = "The super constructor cannot be called because the current class doesn't have a superclass."
  }

  case class CollectionExpected(expression: Expression) extends Error(expression) {
    override def message: String = s"Expected a collection at this position. Got a value of type ${expression.tpe}."
  }
}
