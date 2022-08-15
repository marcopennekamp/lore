package lore.compiler.semantics.expressions

import lore.compiler.core.{CompilationException, Position, Positioned, UniqueKey}
import lore.compiler.semantics.analysis.CapturedVariables
import lore.compiler.semantics.bindings.{LocalVariable, StructConstructorBinding, TypedTermBinding}
import lore.compiler.semantics.expressions.Expression.Literal.LiteralValue
import lore.compiler.semantics.functions.{CallTarget, FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable

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
  // TODO (multi-import): Adopt names and definition order from UntypedExpression.
  // TODO (multi-import): Rename all mentions to an anonymous function with lambda (function).

  sealed abstract class Apply(override val tpe: Type) extends Expression

  /**
    * This expression is used as a stand-in when a compilation error makes it impossible to create a valid expression.
    * The given type should be a best guess as to the actual type of a valid expression.
    */
  case class Hole(tpe: Type, position: Position) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Top-level expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Return(value: Expression, position: Position) extends Expression.Apply(BasicType.Nothing)

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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Access expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A cross-cutting trait signifying the possible access of a variable/property and thus the potential target of an
    * assignment.
    */
  sealed trait Access extends Expression {
    def label: String
    def isMutable: Boolean
    override def toString: String = label
  }

  case class BindingAccess(binding: TypedTermBinding, position: Position) extends Expression.Apply(binding.tpe) with Access {
    override val label: String = binding.toString
    override val isMutable: Boolean = binding.isMutable
  }

  case class MemberAccess(instance: Expression, member: Member, position: Position) extends Expression.Apply(member.tpe) with Access {
    override val label: String = member.name
    override val isMutable: Boolean = member.isMutable
  }

  /**
    * A member access that cannot yet be resolved because the expression's type hasn't been inferred.
    */
  case class UnresolvedMemberAccess(instance: Expression, name: String, tpe: InferenceVariable, position: Position) extends Expression with Access {
    override val label: String = name
    override def isMutable: Boolean = throw CompilationException(s"$this has an undefined mutability.")
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * @param expressions The expressions list must have at least one element. Empty blocks should be populated with a
    *                    single unit value expression.
    * @param tpe         The result type of the block. This is usually the type of the last expression, but not when
    *                    the type expected of the block is Unit. In this case, the block will receive an implicit unit
    *                    value as its last expression after typechecking.
    */
  case class Block(expressions: Vector[Expression], tpe: Type, position: Position) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Literals and Value Constructors.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Literal(value: LiteralValue, position: Position) extends Expression.Apply(value.tpe)

  object Literal {
    /**
      * We use this two-tiered approach to represent literals instead of distinct expressions (IntLiteral, RealLiteral,
      * etc.) because regardless of the type, a literal is treated in the same way across the compiler. This is similar
      * to operations, which are also implemented in two tiers, and cuts down on the number of Expressions.
      */
    trait LiteralValue {
      def tpe: Type
    }

    case class IntValue(value: Long) extends LiteralValue {
      override def tpe: Type = BasicType.Int
    }

    case class RealValue(value: Double) extends LiteralValue {
      override def tpe: Type = BasicType.Real
    }

    case class BooleanValue(value: Boolean) extends LiteralValue {
      override def tpe: Type = BasicType.Boolean
    }

    case class StringValue(value: String) extends LiteralValue {
      override def tpe: Type = BasicType.String
    }

    case class SymbolValue(name: String) extends LiteralValue {
      override def tpe: Type = SymbolType(name)
    }

    def integer(value: Long, position: Position): Literal = Literal(IntValue(value), position)
    def real(value: Double, position: Position): Literal = Literal(RealValue(value), position)
    def boolean(value: Boolean, position: Position): Literal = Literal(BooleanValue(value), position)
    def string(value: String, position: Position): Literal = Literal(StringValue(value), position)
    def symbol(name: String, position: Position): Literal = Literal(SymbolValue(name), position)
  }

  case class Tuple(values: Vector[Expression], position: Position) extends Expression {
    override val tpe: TupleType = if (values.isEmpty) TupleType.UnitType else TupleType(values.map(_.tpe))
  }

  case class AnonymousFunction(
    parameters: Vector[AnonymousFunctionParameter],
    body: Expression,
    position: Position,
  ) extends Expression {
    override val tpe: FunctionType = FunctionType(TupleType(parameters.map(_.tpe)), body.tpe)

    /**
      * Whether the anonymous function only has annotated parameters. This allows the inference algorithm to infer the
      * type of the anonymous function directly.
      */
    lazy val isFullyAnnotated: Boolean = parameters.forall(_.isAnnotated)

    /**
      * All local variables that this anonymous function must capture.
      */
    lazy val capturedVariables: Vector[LocalVariable] = CapturedVariables.findCapturedVariables(this).toVector
  }

  // TODO (multi-import): Do we still need unique keys with the new UntypedExpression IR?
  case class AnonymousFunctionParameter(uniqueKey: UniqueKey, name: String, tpe: Type, position: Position) {
    /**
      * Whether the parameter has a type annotation. Unannotated parameters always have an inference variable as their
      * type.
      */
    lazy val isAnnotated: Boolean = InferenceVariable.isFullyInstantiated(tpe)

    def mapType(f: Type => Type): AnonymousFunctionParameter = this.copy(tpe = f(tpe))
  }

  /**
    * A multi-function typed as a function. It can be passed around like any other function value.
    *
    * @param tpe The [[FunctionType]] of the multi-function value. Typechecking expects `tpe` to be an
    *            [[InferenceVariable]].
    */
  case class MultiFunctionValue(mf: MultiFunctionDefinition, tpe: Type, position: Position) extends Expression

  /**
    * A fixed function instance typed as a function. It can be passed around like any other function value.
    */
  case class FixedFunctionValue(instance: FunctionInstance, position: Position) extends Expression {
    override def tpe: Type = instance.signature.functionType
  }

  /**
    * A struct constructor typed as a function. It can be passed around like any other function value.
    */
  case class ConstructorValue(binding: StructConstructorBinding, structType: StructType, position: Position) extends Expression {
    override def tpe: Type = structType.constructorSignature.functionType
  }

  /**
    * A [[ConstructorValue]] without explicitly specified type arguments, which need to be inferred first.
    *
    * @param tpe This inference variable represents the function type of the constructor value.
    */
  case class UntypedConstructorValue(binding: StructConstructorBinding, tpe: InferenceVariable, position: Position) extends Expression

  /**
    * A list's element type is fully informed by the types of its elements.
    */
  case class ListConstruction(values: Vector[Expression], position: Position) extends Expression {
    val elementType: Type = SumType.construct(values.map(_.tpe))
    override val tpe: Type = ListType(elementType)
  }

  case class MapConstruction(entries: Vector[MapEntry], position: Position) extends Expression {
    val keyType: Type = SumType.construct(entries.map(_.key.tpe))
    val valueType: Type = SumType.construct(entries.map(_.value.tpe))
    override def tpe: Type = MapType(keyType, valueType)

    def withEntries(entries: Vector[(Expression, Expression)]): MapConstruction = this.copy(entries.map(MapEntry.tupled))
  }

  case class MapEntry(key: Expression, value: Expression)

  case class ShapeValue(properties: Vector[ShapeProperty], position: Position) extends Expression {
    override val tpe: Type = ShapeType(properties.map(_.asShapeTypeProperty))

    /**
      * Creates a new shape value with the given property values. The order of properties and values must be
      * compatible!
      */
    def withPropertyValues(values: Vector[Expression]): ShapeValue = {
      this.copy(properties.zip(values).map { case (property, value) => property.copy(value = value) })
    }
  }

  case class ShapeProperty(name: String, value: Expression) {
    def asShapeTypeProperty: ShapeType.Property = ShapeType.Property(name, value.tpe)
  }

  case class PropertyDefaultValue(property: StructProperty, position: Position) extends Expression {
    if (!property.hasDefault) {
      throw CompilationException("A PropertyDefaultValue expression must receive a property with a default value.")
    }

    override def tpe: Type = property.tpe
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (multi-import): Move to the general package, as its used by both UntypedExpressions and Expressions.
  sealed trait UnaryOperator
  object UnaryOperator {
    case object Negation extends UnaryOperator
    case object LogicalNot extends UnaryOperator
  }

  sealed trait BinaryOperator
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

  sealed trait XaryOperator
  object XaryOperator {
    case object Conjunction extends XaryOperator
    case object Disjunction extends XaryOperator
    case object Concatenation extends XaryOperator
  }

  case class UnaryOperation(operator: UnaryOperator, value: Expression, tpe: Type, position: Position) extends Expression
  case class BinaryOperation(operator: BinaryOperator, left: Expression, right: Expression, tpe: Type, position: Position) extends Expression
  case class XaryOperation(operator: XaryOperator, expressions: Vector[Expression], tpe: Type, position: Position) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Call(target: CallTarget, arguments: Vector[Expression], tpe: Type, position: Position) extends Expression


  // TODO (multi-import): Trait Call and case classes for each call target.


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A `cond` expression must always be total. If the code doesn't have a total case, the transformation must generate
    * a last, total case which evaluates to unit. This guarantee simplifies typing and assembly.
    */
  case class Cond(cases: Vector[CondCase], position: Position) extends Expression {
    override val tpe: Type = SumType.construct(cases.map(_.body.tpe))

    def withCases(pairs: Vector[(Expression, Expression)]): Cond = this.copy(pairs.map(CondCase.tupled))
  }

  object Cond {
    def apply(conditions: Vector[Expression], bodies: Vector[Expression], position: Position): Cond = {
      Cond(conditions.zip(bodies).map(CondCase.tupled), position)
    }
  }

  case class CondCase(condition: Expression, body: Expression) {
    val isTotalCase: Boolean = condition match {
      case Literal(Literal.BooleanValue(true), _) => true
      case _ => false
    }
  }

  sealed trait Loop extends Expression {
    def body: Expression
    def tpe: Type = ListType(body.tpe)
  }

  case class WhileLoop(condition: Expression, body: Expression, position: Position) extends Loop

  // TODO: Can't we break down a for loop into a while loop when transforming UntypedExpression to Expression, or even
  //       before (Node -> UntypedExpression)? We can achieve this either with a universal `flat_map` concept OR an
  //       iterator approach.
  case class ForLoop(extractors: Vector[Extractor], body: Expression, position: Position) extends Loop {
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
  case class Extractor(variable: LocalVariable, collection: Expression)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type ascriptions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO (multi-import): No need for this, only in UntypedExpression.
  case class Ascription(value: Expression, expectedType: Type, position: Position) extends Expression.Apply(expectedType)
}
