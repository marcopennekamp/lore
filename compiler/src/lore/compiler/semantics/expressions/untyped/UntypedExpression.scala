package lore.compiler.semantics.expressions.untyped

import lore.compiler.core.{Position, Positioned}
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.semantics.bindings.{StructConstructorBinding, TermBinding, TypedTermBinding, UntypedLocalVariable}
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, Operator, UnaryOperator, XaryOperator}
import lore.compiler.semantics.functions.{FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.modules.MultiReference
import lore.compiler.types.{StructProperty, Type}

/**
  * This is an intermediate representation of expressions that sits between scope resolution/general transformation and
  * typing.
  *
  * The need for this IR comes from the issue that some expressions such as [[lore.compiler.semantics.expressions.Expression.MemberAccess]]
  * and [[lore.compiler.semantics.expressions.Expression.MultiFunctionValue]] require type information for other
  * properties to be resolved, such as the MemberAccess's member. There are two workarounds:
  *
  *   1. Make Expressions mutable and attach these properties during typing. This clashes with the need to attempt but
  *      not necessarily commit to a typing path during dispatch typing.
  *   2. Introduce expressions such as an "UnresolvedMemberAccess" which need to be specially transformed after typing.
  *      The issue here is that the assembly phase then has to take care not to accept these unresolved expressions,
  *      and in general the compiler's type system cannot provide as many guarantees as compared to a transformation
  *      between two distinct IRs.
  *
  * Both workarounds have their issues. The UntypedExpression representation is more verbose, but cleaner and
  * ultimately preferable.
  */
trait UntypedExpression extends Positioned

object UntypedExpression {

  case class UntypedHole(fallbackType: Type, position: Position) extends UntypedExpression

  case class UntypedTypeAscription(
    expression: UntypedExpression,
    expectedType: Type,
    position: Position,
  ) extends UntypedExpression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Values.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (multi-import): This untyped IntValue could help us to allow ints in real positions, e.g. `[5.0, 3]` typed as
  //                      `Real`.
  case class UntypedIntValue(value: Long, position: Position) extends UntypedExpression
  case class UntypedRealValue(value: Double, position: Position) extends UntypedExpression
  case class UntypedBooleanValue(value: Boolean, position: Position) extends UntypedExpression
  case class UntypedStringValue(value: String, position: Position) extends UntypedExpression
  case class UntypedSymbolValue(name: String, position: Position) extends UntypedExpression

  case class UntypedTupleValue(
    elements: Vector[UntypedExpression],
    position: Position
  ) extends UntypedExpression

  case class UntypedLambdaValue(
    parameters: Vector[UntypedLambdaParameter],
    body: UntypedExpression,
    position: Position,
  ) extends UntypedExpression {
    /**
      * Whether the lambda function only has annotated parameters. This allows the inference algorithm to infer the
      * type of the function directly.
      */
    lazy val isFullyAnnotated: Boolean = parameters.forall(_.typeAnnotation.isDefined)
  }

  case class UntypedLambdaParameter(
    variable: UntypedLocalVariable,
    typeAnnotation: Option[Type],
    position: Position,
  )

  // TODO (multi-import): The idea is that the multi-reference supports both unambiguous and ambiguous multi-functions.
  case class UntypedMultiFunctionValue(
    mfs: MultiReference[MultiFunctionDefinition],
    position: Position,
  ) extends UntypedExpression

  case class UntypedFixedFunctionValue(
    instance: FunctionInstance,
    position: Position,
  ) extends UntypedExpression

  // TODO (multi-import): Support explicit type arguments.
  case class UntypedConstructorValue(
    binding: StructConstructorBinding,
    position: Position,
  ) extends UntypedExpression

  case class UntypedListValue(
    elements: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedExpression

  // TODO (maps): Support map values.

  case class UntypedShapeValue(
    properties: Vector[UntypedShapeProperty],
    position: Position,
  ) extends UntypedExpression

  case class UntypedShapeProperty(
    name: String,
    value: UntypedExpression,
    position: Position,
  )

  case class UntypedPropertyDefaultValue(
    property: StructProperty,
    position: Position,
  ) extends UntypedExpression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operators and calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait UntypedOperation extends UntypedExpression {
    def operator: Operator
    def operands: Vector[UntypedExpression]
  }

  case class UntypedUnaryOperation(
    operator: UnaryOperator,
    operand: UntypedExpression,
    position: Position,
  ) extends UntypedOperation {
    override def operands: Vector[UntypedExpression] = Vector(operand)
  }

  case class UntypedBinaryOperation(
    operator: BinaryOperator,
    operand1: UntypedExpression,
    operand2: UntypedExpression,
    position: Position,
  ) extends UntypedOperation {
    override def operands: Vector[UntypedExpression] = Vector(operand1, operand2)
  }

  case class UntypedXaryOperation(
    operator: XaryOperator,
    operands: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedOperation

  trait UntypedCall extends UntypedExpression {
    def arguments: Vector[UntypedExpression]
  }

  // TODO (multi-import): The idea is that the multi-reference supports both unambiguous and ambiguous multi-functions.
  case class UntypedMultiFunctionCall(
    target: MultiReference[MultiFunctionDefinition],
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedValueCall(
    target: UntypedExpression,
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedConstructorCall(
    target: StructConstructorBinding,
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedIntrinsicCall(
    target: PoemIntrinsic,
    arguments: Vector[UntypedExpression],
    tpe: Type,
    position: Position,
  ) extends UntypedCall

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variables and members.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (multi-import): The variable needs a position!
  case class UntypedVariableDeclaration(
    variable: UntypedLocalVariable,
    value: UntypedExpression,
    typeAnnotation: Option[Type],
    position: Position,
  ) extends UntypedExpression

  case class UntypedAssignment(
    target: UntypedAccess,
    value: UntypedExpression,
    position: Position,
  ) extends UntypedExpression

  sealed trait UntypedAccess extends UntypedExpression {
    def label: String

    override def toString: String = label
  }

  /**
    * @param binding The binding is a [[TermBinding]], not a [[TypedTermBinding]], because
    *
    *                TODO (multi-import): The untyped nature of the binding might allow us to move access coercion of multi-functions
    *                to the typing phase, because this binding access could carry a multi-function binding. That
    *                would mean that we don't need an UntypedMultiFunctionValue.
    */
  case class UntypedBindingAccess(
    binding: TermBinding,
    position: Position,
  ) extends UntypedExpression with UntypedAccess {
    override val label: String = binding.toString
  }

  case class UntypedMemberAccess(
    instance: UntypedExpression,
    name: String,
    position: Position,
  ) extends UntypedExpression with UntypedAccess {
    override val label: String = name
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Control expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class UntypedReturn(
    value: UntypedExpression,
    position: Position,
  ) extends UntypedExpression

  case class UntypedBlock(
    expressions: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedExpression

  case class UntypedCond(
    cases: Vector[UntypedCondCase],
    position: Position,
  ) extends UntypedExpression

  case class UntypedCondCase(
    condition: UntypedExpression,
    body: UntypedExpression,
  )

  sealed trait UntypedLoop extends UntypedExpression {
    def body: UntypedExpression
  }

  case class UntypedWhileLoop(
    condition: UntypedExpression,
    body: UntypedExpression,
    position: Position,
  ) extends UntypedLoop

  case class UntypedForLoop(
    extractors: Vector[UntypedExtractor],
    body: UntypedExpression,
    position: Position,
  ) extends UntypedLoop

  case class UntypedExtractor(
    variable: UntypedLocalVariable,
    collection: UntypedExpression,
  )

}
