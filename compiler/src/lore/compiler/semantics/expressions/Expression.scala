package lore.compiler.semantics.expressions

import lore.compiler.core.{CompilationException, Position, Positioned, UniqueKey}
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.semantics.analysis.CapturedVariables
import lore.compiler.semantics.bindings.{LocalVariable, StructConstructorBinding, TypedTermBinding}
import lore.compiler.semantics.functions.{FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.types._

// TODO (multi-import): Move to expressions.typed package.

sealed trait Expression extends Positioned {
  def tpe: Type

  /**
    * Whether the result of this expression is used by its parent expression. If the expression is unused, certain
    * instructions may be omitted during assembly.
    *
    * `isUsed` is set by [[lore.compiler.transformation.UsageAnalyzer]] during the transformation stage.
    *
    * TODO (multi-import): Can we roll this into the UntypedExpression -> Expression transformation?
    */
  var isUsed: Boolean = true

  def isUnused: Boolean = !isUsed

  def setUnused(): Unit = {
    isUsed = false
  }
}

object Expression {

  sealed abstract class Apply(override val tpe: Type) extends Expression

  /**
    * This expression is used as a stand-in when a compilation error makes it impossible to create a valid expression.
    * The given type should be a best guess as to the actual type of a valid expression.
    */
  case class Hole(tpe: Type, position: Position) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Values.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class IntValue(value: Long, position: Position) extends Expression.Apply(BasicType.Int)
  case class RealValue(value: Double, position: Position) extends Expression.Apply(BasicType.Real)
  case class BooleanValue(value: Boolean, position: Position) extends Expression.Apply(BasicType.Boolean)
  case class StringValue(value: String, position: Position) extends Expression.Apply(BasicType.String)
  case class SymbolValue(name: String, position: Position) extends Expression.Apply(SymbolType(name))

  case class TupleValue(
    elements: Vector[Expression],
    position: Position,
  ) extends Expression {
    override val tpe: TupleType = if (elements.isEmpty) TupleType.UnitType else TupleType(elements.map(_.tpe))
  }

  // TODO (multi-import): Rename all mentions to an anonymous function with lambda (function/value).
  case class LambdaValue(
    parameters: Vector[LambdaParameter],
    body: Expression,
    position: Position,
  ) extends Expression {
    override val tpe: FunctionType = FunctionType(TupleType(parameters.map(_.variable.tpe)), body.tpe)

    /**
      * All local variables that this anonymous function must capture.
      */
    lazy val capturedVariables: Vector[LocalVariable] = CapturedVariables.findCapturedVariables(this).toVector
  }

  case class LambdaParameter(
    variable: LocalVariable,
    position: Position,
  )

  /**
    * A multi-function typed as a function. It can be passed around like any other function value.
    */
  case class MultiFunctionValue(
    mf: MultiFunctionDefinition,
    tpe: FunctionType,
    position: Position,
  ) extends Expression

  /**
    * A fixed function instance typed as a function. It can be passed around like any other function value.
    */
  case class FixedFunctionValue(
    instance: FunctionInstance,
    position: Position,
  ) extends Expression {
    override def tpe: FunctionType = instance.signature.functionType
  }

  /**
    * A struct constructor typed as a function. It can be passed around like any other function value.
    */
  case class ConstructorValue(
    binding: StructConstructorBinding,
    structType: StructType,
    position: Position,
  ) extends Expression {
    override def tpe: FunctionType = structType.constructorSignature.functionType
  }

  case class ListValue(
    elements: Vector[Expression],
    position: Position,
  ) extends Expression {
    val elementType: Type = SumType.construct(elements.map(_.tpe))
    override val tpe: Type = ListType(elementType)
  }

  // TODO (maps): Support map values.

//  case class MapConstruction(entries: Vector[MapEntry], position: Position) extends Expression {
//    val keyType: Type = SumType.construct(entries.map(_.key.tpe))
//    val valueType: Type = SumType.construct(entries.map(_.value.tpe))
//
//    override def tpe: Type = MapType(keyType, valueType)
//
//    def withEntries(entries: Vector[(Expression, Expression)]): MapConstruction = this.copy(entries.map(MapEntry.tupled))
//  }
//
//  case class MapEntry(key: Expression, value: Expression)

  case class ShapeValue(
    properties: Vector[ShapeProperty],
    position: Position,
  ) extends Expression {
    override val tpe: Type = ShapeType(properties.map(_.asShapeTypeProperty))

    /**
      * Creates a new shape value with the given property values. The order of properties and values must be
      * compatible!
      */
    def withPropertyValues(values: Vector[Expression]): ShapeValue = {
      this.copy(properties.zip(values).map { case (property, value) => property.copy(value = value) })
    }
  }

  case class ShapeProperty(
    name: String,
    value: Expression,
    position: Position,
  ) {
    def asShapeTypeProperty: ShapeType.Property = ShapeType.Property(name, value.tpe)
  }

  case class PropertyDefaultValue(
    property: StructProperty,
    position: Position,
  ) extends Expression {
    if (!property.hasDefault) {
      throw CompilationException("A PropertyDefaultValue expression must receive a property with a default value.")
    }

    override def tpe: Type = property.tpe
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operators and calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (multi-import): Move to the general package, as its used by both UntypedExpressions and Expressions.
  sealed trait Operator

  sealed trait UnaryOperator extends Operator
  object UnaryOperator {
    case object Negation extends UnaryOperator
    case object LogicalNot extends UnaryOperator
  }

  sealed trait BinaryOperator extends Operator
  object BinaryOperator {
    case object Addition extends BinaryOperator
    case object Subtraction extends BinaryOperator
    case object Multiplication extends BinaryOperator
    case object Division extends BinaryOperator
    case object Equals extends BinaryOperator
    case object LessThan extends BinaryOperator
    case object LessThanEquals extends BinaryOperator
    case object Append extends BinaryOperator
  }

  sealed trait XaryOperator extends Operator
  object XaryOperator {
    case object Conjunction extends XaryOperator
    case object Disjunction extends XaryOperator
    case object Concatenation extends XaryOperator
  }

  case class UnaryOperation(
    operator: UnaryOperator,
    value: Expression,
    tpe: Type,
    position: Position,
  ) extends Expression

  case class BinaryOperation(
    operator: BinaryOperator,
    left: Expression,
    right: Expression,
    tpe: Type,
    position: Position,
  ) extends Expression

  case class XaryOperation(
    operator: XaryOperator,
    operands: Vector[Expression],
    tpe: Type,
    position: Position,
  ) extends Expression

  trait Call extends Expression {
    def arguments: Vector[Expression]
  }

  case class MultiFunctionCall(
    target: FunctionInstance,
    arguments: Vector[Expression],
    position: Position,
  ) extends Call {
    override def tpe: Type = target.signature.outputType
  }

  case class ValueCall(
    target: Expression,
    arguments: Vector[Expression],
    tpe: Type,
    position: Position,
  ) extends Call

  case class ConstructorCall(
    target: StructConstructorBinding,
    arguments: Vector[Expression],
    tpe: Type,
    position: Position,
  ) extends Call

  case class IntrinsicCall(
    target: PoemIntrinsic,
    arguments: Vector[Expression],
    tpe: Type,
    position: Position,
  ) extends Call

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variables and members.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * TODO: Do we even need to generate VariableDeclarations from untyped Assignments?
    *
    * @param typeAnnotation The type that the variable declaration was annotated with.
    */
  case class VariableDeclaration(
    variable: LocalVariable,
    value: Expression,
    typeAnnotation: Option[Type], // TODO (multi-import): This can be removed.
    position: Position,
  ) extends Expression.Apply(TupleType.UnitType)

  case class Assignment(
    target: Expression.Access,
    value: Expression,
    position: Position,
  ) extends Expression.Apply(TupleType.UnitType)

  /**
    * A cross-cutting trait signifying the possible access of a variable/property and thus the potential target of an
    * assignment.
    */
  sealed trait Access extends Expression {
    def label: String
    def isMutable: Boolean

    override def toString: String = label
  }

  case class BindingAccess(
    binding: TypedTermBinding,
    position: Position,
  ) extends Expression.Apply(binding.tpe) with Access {
    override val label: String = binding.toString
    override val isMutable: Boolean = binding.isMutable
  }

  case class MemberAccess(
    instance: Expression,
    member: Member,
    position: Position,
  ) extends Expression.Apply(member.tpe) with Access {
    override val label: String = member.name
    override val isMutable: Boolean = member.isMutable
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Control expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Return(
    value: Expression,
    position: Position,
  ) extends Expression.Apply(BasicType.Nothing)

  /**
    * @param expressions The expressions list must have at least one element. Empty blocks should be populated with a
    *                    single unit value expression.
    */
  case class Block(
    expressions: Vector[Expression],
    position: Position,
  ) extends Expression {
    override val tpe: Type = expressions.last.tpe
  }

  /**
    * A `cond` expression must always be total. If the code doesn't have a total case, the transformation must generate
    * a last, total case which evaluates to unit.
    */
  case class Cond(
    cases: Vector[CondCase],
    position: Position,
  ) extends Expression {
    override val tpe: Type = SumType.construct(cases.map(_.body.tpe))

    def withCases(pairs: Vector[(Expression, Expression)]): Cond = this.copy(pairs.map(CondCase.tupled))
  }

  object Cond {
    def apply(conditions: Vector[Expression], bodies: Vector[Expression], position: Position): Cond = {
      Cond(conditions.zip(bodies).map(CondCase.tupled), position)
    }
  }

  case class CondCase(
    condition: Expression,
    body: Expression,
  ) {
    val isTotalCase: Boolean = condition match {
      case BooleanValue(true, _) => true
      case _ => false
    }
  }

  sealed trait Loop extends Expression {
    def body: Expression
    lazy val tpe: Type = ListType(body.tpe)
  }

  case class WhileLoop(
    condition: Expression,
    body: Expression,
    position: Position,
  ) extends Loop

  // TODO: Can't we break down a for loop into a while loop when transforming UntypedExpression to Expression, or even
  //       before (Node -> UntypedExpression)? We can achieve this either with a universal `flat_map` concept OR an
  //       iterator approach.
  case class ForLoop(
    extractors: Vector[Extractor],
    body: Expression,
    position: Position,
  ) extends Loop {
    /**
      * Creates a for-loop expression with the given collection values. The order of extractors and collections must be
      * compatible!
      */
    def withCollections(collections: Vector[Expression]): ForLoop = {
      this.copy(extractors.zip(collections).map { case (extractor, collection) => extractor.copy(collection = collection) })
    }
  }

  /**
    * Note that `variable` contains the element type of the extractor, which must be inferred and rehydrated later.
    */
  case class Extractor(
    variable: LocalVariable,
    collection: Expression,
  )

}
