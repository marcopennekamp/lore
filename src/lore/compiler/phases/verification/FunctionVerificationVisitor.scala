package lore.compiler.phases.verification

import lore.ast.ExprNode.AddressNode
import lore.ast.visitor.VerificationStmtVisitor
import lore.ast.{CallNode, ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.{Error, Position}
import lore.compiler.phases.verification.FunctionVerification.IllegallyTypedExpression
import lore.compiler.{Compilation, Fragment, Registry, TypeExpressionEvaluator}
import lore.definitions.{CallTarget, ClassDefinition, FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.types._

private[verification] class FunctionVerificationVisitor(
  /**
    * The function or constructor for which we want to infer types.
    */
  val callTarget: CallTarget,
  /**
    * The class that owns the constructor IF the signature represents a constructor.
    */
  val classDefinition: Option[ClassDefinition],
)(implicit registry: Registry, fragment: Fragment) extends VerificationStmtVisitor {
  import ExprNode._
  import FunctionVerificationVisitor._
  import StmtNode._
  import TopLevelExprNode._

  /**
    * The function verification context used by the visitor to open/close scopes, register yields, and so on.
    */
  val context = new FunctionVerificationContext(callTarget.signature)

  /**
    * Whether the given statement's inferred type is a subtype of one of the expected types.
    */
  private def havingSubtype(statement: StmtNode, supertypes: Type*): Verification = {
    if (!supertypes.exists(expected => Subtyping.isSubtype(statement.inferredType, expected))) {
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
    * and also types it.
    */
  private def typeCall(node: CallNode, target: CallTarget, arguments: List[ExprNode]): Verification = {
    adheringToSignature(arguments, target.signature, node.position).flatMap { _ =>
      node.target = target
      node.typed(target.signature.outputType)
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
      // TODO: Check that a return is the last statement in the block. This effectively disallows dead code after
      //       a return statement.
      // TODO: Disallow constructions such as `if ({ return 0 }) a else b`. Returning should not be possible from
      //       blocks that are in an expression position. We might have to add such a notion to blocks.
      // TODO: Disallow returns in constructor bodies.
      havingSubtype(expr, callTarget.signature.outputType).flatMap { _ =>
        node.typed(NothingType)
      }
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
                case List(target) => typeCall(node, target, arguments)
              }
            }
          }
      }
    case node@FixedFunctionCallNode(name, typeExpressions, arguments) =>
      typeExpressions.map(TypeExpressionEvaluator.evaluate).simultaneous.flatMap { types =>
        registry.resolveExactFunction(name, types, node.position).flatMap { function =>
          typeCall(node, function, arguments)
        }
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
        //       HAVE options just yet. This also needs to become part of the spec before it's implemented, IF we
        //       implement this feature.
        val resultType = Subtyping.leastUpperBound(onTrue.inferredType, onFalse.inferredType)
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
      node.typed(expressions.map(_.inferredType).foldLeft(NothingType: Type)(Subtyping.leastUpperBound))
    case MapNode(entries) =>
      val keyType = entries.map(_.key.inferredType).foldLeft(NothingType: Type)(Subtyping.leastUpperBound)
      val valueType = entries.map(_.value.inferredType).foldLeft(NothingType: Type)(Subtyping.leastUpperBound)
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

  override def before: PartialFunction[StmtNode, Unit] = {
    case ExprNode.RepeatWhileNode(_, _, _) | ExprNode.IterationNode(_, _) =>
      // TODO: Put a new yield context on the stack.
    case ExprNode.BlockNode(_) => context.openScope()
  }
}

private[verification] object FunctionVerificationVisitor {
  case class ImmutableAssignment(addressNode: AddressNode)(implicit fragment: Fragment) extends Error(addressNode) {
    override def message = s"The variable or property you are trying to assign to is immutable."
  }

  case class EmptyFit(mf: MultiFunctionDefinition, callPos: Position) extends Error(callPos) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot " +
      s" find a function of that name that would accept the given arguments."
  }

  case class AmbiguousCall(mf: MultiFunctionDefinition, min: List[FunctionDefinition], callPos: Position) extends Error(callPos) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an ambiguous min-set." +
      s" That is, we are finding TOO MANY functions that would accept the given arguments: ${min.mkString(", ")}."
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
