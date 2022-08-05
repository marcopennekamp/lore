package lore.compiler.semantics.expressions.untyped

import lore.compiler.core.{Position, Positioned, UniqueKey}
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.semantics.bindings.{LocalVariable, StructConstructorBinding, TypedTermBinding}
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
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

  // TODO (multi-import): Provide a suggested type for the hole?
  case class UntypedHole(position: Position) extends UntypedExpression

  case class TypeAscription(
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
  ) extends UntypedExpression

  case class UntypedLambdaParameter(
    uniqueKey: UniqueKey,
    name: String,
    typeAnnotation: Option[Type],
    position: Position,
  )

  // TODO (multi-import): The idea is that the multi-reference supports both unambiguous and ambiguous multi-functions.
  case class UntypedMultiFunctionValue(
    multiReference: MultiReference[MultiFunctionDefinition],
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
  case class UntypedUnaryOperation(
    operator: UnaryOperator,
    value: UntypedExpression,
    position: Position,
  ) extends UntypedExpression

  case class UntypedBinaryOperation(
    operator: BinaryOperator,
    left: UntypedExpression,
    right: UntypedExpression,
    position: Position,
  ) extends UntypedExpression

  case class UntypedXaryOperation(
    operator: XaryOperator,
    operands: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedExpression

  trait UntypedCall extends UntypedExpression {
    def arguments: Vector[UntypedExpression]
  }

  // TODO (multi-import): The idea is that the multi-reference supports both unambiguous and ambiguous multi-functions.
  case class UntypedMultiFunctionCall(
    multiReference: MultiReference[MultiFunctionDefinition],
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedValueCall(
    target: UntypedExpression,
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedConstructorCall(
    binding: StructConstructorBinding,
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  case class UntypedIntrinsicCall(
    intrinsic: PoemIntrinsic,
    arguments: Vector[UntypedExpression],
    position: Position,
  ) extends UntypedCall

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variables and members.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class UntypedVariableDeclaration(
    variable: LocalVariable, // TODO (multi-import): Untyped local variable?
    value: UntypedExpression,
    typeAnnotation: Option[Type],
    position: Position,
  )

  case class UntypedAssignment(
    target: UntypedAccess,
    value: UntypedExpression,
    position: Position,
  ) extends UntypedExpression

  sealed trait UntypedAccess extends UntypedExpression {
    def label: String

    override def toString: String = label
  }

  case class UntypedBindingAccess(
    binding: TypedTermBinding,
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
    variable: LocalVariable,  // TODO (multi-import): Untyped local variable?
    collection: UntypedExpression,
  )

}
