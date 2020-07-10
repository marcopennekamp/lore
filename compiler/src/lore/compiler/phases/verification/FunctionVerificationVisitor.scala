package lore.compiler.phases.verification

import lore.compiler.ast.ExprNode.AddressNode
import lore.compiler.ast.visitor.VerificationStmtVisitor
import lore.compiler.ast.{CallNode, ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Fragment, Registry, TypeScope}
import lore.compiler.feedback.{Error, Position}
import lore.compiler.functions._
import lore.compiler.phases.verification.FunctionVerification.IllegallyTypedExpression
import lore.compiler.structures.ClassDefinition
import lore.compiler.types._

private[verification] class FunctionVerificationVisitor(
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
  classDefinition: Option[ClassDefinition],
)(implicit registry: Registry, fragment: Fragment) extends VerificationStmtVisitor {
  import ExprNode._
  import FunctionVerificationVisitor._
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
  val context = new FunctionVerificationContext(topSignature)

  /**
    * Whether the given statement's inferred type is a subtype of one of the expected types.
    */
  private def havingSubtype(statement: StmtNode, supertypes: Type*): Verification = {
    if (!supertypes.exists(expected => statement.inferredType <= expected)) {
      Compilation.fail(IllegallyTypedExpression(statement, supertypes.toList))
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

  case class WrongNumberOfArguments(signature: FunctionSignature, callPos: Position) extends Error(callPos) {
    override def message: String = s"The function/constructor ${signature.name} was called with the wrong number of arguments." +
      s" Expected: ${signature.parameters.size}."
  }

  /**
    * Checks that the given arguments adhere to the given signature.
    *
    * We are assuming that the signature is fixed, so don't use this for dispatched functions.
    */
  private def adheringToSignature(arguments: List[ExprNode], signature: FunctionSignature, callSite: Position): Verification = {
    val parameterTypes = signature.parameters.map(_.tpe)
    if (parameterTypes.size != arguments.size) {
      Compilation.fail(WrongNumberOfArguments(signature, callSite))
    } else {
      parameterTypes.zip(arguments).map { case (parameterType, argument) =>
        havingSubtype(argument, parameterType)
      }.simultaneous.verification
    }
  }

  /**
    * Checks that the given call target can be called with the given arguments, then assigns the target to the node
    * and also types it. Do NOT use this for dispatched functions!
    */
  private def typeCall(node: CallNode[InternalCallTarget], target: InternalCallTarget, arguments: List[ExprNode]): Verification = {
    adheringToSignature(arguments, target.signature, node.position).flatMap { _ =>
      assignTarget(node, target)
    }
  }

  /**
    * Assigns the target to the node and types it with the target's output type.
    */
  private def assignTarget(node: CallNode[InternalCallTarget], target: InternalCallTarget): Verification = {
    node.target = target
    node.typed(target.signature.outputType)
  }

  private def typeLoop(node: LoopNode): Verification = {
    val bodyType = node.body.inferredType
    val loopType = if (bodyType == ProductType.UnitType) {
      ProductType.UnitType
    } else {
      ListType(bodyType)
    }
    node.typed(loopType)
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
      havingSubtype(expr, topSignature.outputType).flatMap { _ =>
        node.typed(NothingType)
      }

    // Variables.
    case node@VariableNode(name) =>
      // TODO: We will also need access to global variables if we introduce those into Lore.
      // TODO: Once we treat functions as values, we will have to make this even more complicated by also
      //       considering function names.
      context.currentScope.resolve(name, node.position).flatMap { variable =>
        node.setVariable(variable)
        node.typed(variable.tpe)
      }
    case node@PropertyAccessNode(instance, name) =>
      MemberExplorer.find(name, instance.inferredType, node.position).flatMap { member =>
        node.member = member
        node.typed(member.tpe)
      }
    case node@VariableDeclarationNode(name, isMutable, maybeTypeNode, value) =>
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
          node.setVariable(localVariable)
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

    // Function calls.
    case node@SimpleCallNode(name, qualifier, arguments) =>
      registry.resolveConstructor(name, qualifier, node.position).flatMap {
        constructor => typeCall(node, constructor, arguments)
      } recover {
        // Note that the recover might attempt to catch other errors introduced by typeCall, but we are matching only
        // on the one specific error that will be raised by resolveConstructor, which gives us the guarantee that we
        // aren't recovering from an issue within typeCall. Obviously, if we can find a proper constructor and just
        // typeCall fails, we should NOT fallback to a function.
        case errors@List(Registry.TypeNotFound(className, _)) if name == className =>
          // If we couldn't find a CLASS for the constructor, qualifier must be None. In that case, we can
          // continue and look for a function.
          if (qualifier.isDefined) Compilation.fail(errors: _*)
          else {
            registry.resolveMultiFunction(name, node.position).flatMap { mf =>
              val inputType = ProductType(arguments.map(_.inferredType))
              mf.min(inputType) match {
                case Nil => Compilation.fail(EmptyFit(mf, node.position))
                case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, min, node.position))
                case List(functionDefinition) =>
                  functionDefinition.instantiate(inputType).flatMap { instance =>
                    assignTarget(node, instance)
                  }
              }
            }
          }
      }
    case node@FixedFunctionCallNode(name, typeExpressions, arguments) =>
      for {
        types <- typeExpressions.map(TypeExpressionEvaluator.evaluate).simultaneous
        function <- registry.resolveExactFunction(name, types, node.position)
        // We need to check the arguments against the signature here because we didn't get the function
        // via the argument types.
        instance <- function.instantiate(ProductType(types))
      } yield typeCall(node, instance, arguments)
    case node@DynamicCallNode(resultType, arguments) =>
      (
        // The first argument to the dynamic call must be a constant function name.
        arguments.headOption match {
          case Some(StringLiteralNode(name)) => Compilation.succeed(name)
          case _ => Compilation.fail(DynamicFunctionNameExpected(node))
        },
        TypeExpressionEvaluator.evaluate(resultType),
      ).simultaneous.flatMap { case (name, resultType) =>
        node.target = DynamicCallTarget(name, resultType)
        node.typed(resultType)
      }
    case node@ConstructorCallNode(name, arguments) =>
      // Continue with a constructor this.name. We have already verified that only a constructor can contain a
      // continuation, so we can safely get the class definition here as part of the contract of this visitor.
      assert(classDefinition.isDefined)
      val cl = classDefinition.get
      registry.resolveConstructor(cl, name, node.position).flatMap(constructor => typeCall(node, constructor, arguments))
    case ConstructNode(arguments, _) =>
      // We just have to check whether the arguments adhere to the construct signature. The visitor will already
      // have checked the potential withSuper constructor call.
      assert(classDefinition.isDefined)
      val cl = classDefinition.get
      adheringToSignature(arguments, cl.constructSignature, node.position).flatMap { _ =>
        node.typed(cl.constructSignature.outputType)
      }

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
    case node@RepetitionNode(condition, _) =>
      havingSubtype(condition, BasicType.Boolean).flatMap { _ =>
        typeLoop(node)
      }

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
      // TODO: Warn if the result type is Any?
      havingSubtype(condition, BasicType.Boolean).flatMap { _ =>
        // TODO: If only one branch supplies a value, return an OPTION of the evaluated type. Of course, we don't
        //       HAVE options just yet. This also needs to become part of the spec before it's implemented, IF we
        //       implement this feature.
        val resultType = LeastUpperBound.leastUpperBound(onTrue.inferredType, onFalse.inferredType)
        node.typed(resultType)
      }

    // Blocks.
    case BlockNode(statements) =>
      // This is AFTER the block has been visited. The scope has already been opened and needs to be closed.
      context.closeScope()
      node.typed(statements.lastOption.map(_.inferredType).getOrElse(ProductType.UnitType))

    // Literal constructions.
    case TupleNode(expressions) => node.typed(ProductType(expressions.map(_.inferredType)))
    case ListNode(expressions) =>
      // If we type empty lists as [Nothing], we can assign this empty list to any kind of list, which makes
      // coders happy. :) Hence the default value in the fold.
      val elementType = expressions.map(_.inferredType).foldLeft(NothingType: Type)(LeastUpperBound.leastUpperBound)
      node.typed(ListType(elementType))
    case MapNode(entries) =>
      val keyType = entries.map(_.key.inferredType).foldLeft(NothingType: Type)(LeastUpperBound.leastUpperBound)
      val valueType = entries.map(_.value.inferredType).foldLeft(NothingType: Type)(LeastUpperBound.leastUpperBound)
      node.typed(MapType(keyType, valueType))

    // Xary operations.
    case ConjunctionNode(expressions) => typeXaryBooleans(node, expressions)
    case DisjunctionNode(expressions) => typeXaryBooleans(node, expressions)
    case ConcatenationNode(expressions) =>
      // TODO: Do we need to make sure that each non-string expression has a sort of toString function implementation?
      //       Or do we just implement a basic implementation for Any? This could be some of the first Lore code that
      //       we write...
      node.typed(BasicType.String)
  }

  override def visitIteration(node: IterationNode)(extractors: List[(String, Unit)], visitBody: () => Verification): Verification = {
    // TODO: Alternative solution: Add a function visitExtractor which visits the extractor nodes first. Then we can
    //       open the scope and loop context in before, add each extractor to the scope in visitExtractor, and clean
    //       up the scope and list context in this method, visit iteration. (Or we can go back to pattern-matching
    //       up in the verify method again.)
    // TODO: Also consider using a custom Applicator instead.
    // Before we visit the body, we have to push a new scope and later, once extractors have been evaluated, also
    // a new loop context.
    context.openScope()
    val scope = context.currentScope
    node.extractors.map { extractor =>
      (extractor.collection.inferredType match {
        case ListType(element) => Compilation.succeed(element)
        case MapType(key, value) => Compilation.succeed(ProductType(List(key, value)))
        case _ => Compilation.fail(CollectionExpected(extractor.collection))
      }).flatMap { elementType =>
        val localVariable = LocalVariable(extractor.variableName, elementType, isMutable = false)
        scope.register(localVariable, extractor.position).map(_ => extractor.setVariable(localVariable))
      }
    }.simultaneous.flatMap { _ =>
      visitBody().flatMap { _ =>
        context.closeScope() // We have to close the scope that we opened for the extractors.
        typeLoop(node)
      }
    }
  }

  override def before: PartialFunction[StmtNode, Unit] = {
    case ExprNode.BlockNode(_) => context.openScope()
  }
}

private[verification] object FunctionVerificationVisitor {
  case class ImmutableAssignment(addressNode: AddressNode)(implicit fragment: Fragment) extends Error(addressNode) {
    override def message = s"The variable or property you are trying to assign to is immutable."
  }

  case class EmptyFit(mf: MultiFunctionDefinition, callPos: Position) extends Error(callPos) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot" +
      s" find a function of that name that would accept the given arguments."
  }

  case class AmbiguousCall(mf: MultiFunctionDefinition, min: List[FunctionDefinition], callPos: Position) extends Error(callPos) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an ambiguous min-set." +
      s" That is, we are finding TOO MANY functions that would accept the given arguments: ${min.mkString(", ")}."
  }

  case class DynamicFunctionNameExpected(node: ExprNode.DynamicCallNode)(implicit fragment: Fragment) extends Error(node) {
    override def message: String = "Dynamic calls require a string literal as their first argument, which represents the" +
      " name of the function. Since the name must be available at compile-time, it must be a constant."
  }

  case class CollectionExpected(node: ExprNode)(implicit fragment: Fragment) extends Error(node) {
    override def message: String = s"Expected a collection at this position. Got $node instead, which has type ${node.inferredType}."
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
