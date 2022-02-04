package lore.compiler.semantics.expressions

import lore.compiler.core.{CompilationException, Position, Positioned}
import lore.compiler.semantics.analysis.LocalizedExpression
import lore.compiler.semantics.expressions.Expression.Literal.LiteralValue
import lore.compiler.semantics.functions.{CallTarget, FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.semantics.scopes.{LocalVariable, StructConstructorBinding, TypedBinding}
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable

sealed trait Expression extends Positioned {
  def tpe: Type

  /**
    * Whether this expression is localized, meaning that it doesn't access multi-functions, constructors, objects, or
    * global variables.
    */
  lazy val isLocalized: Boolean = LocalizedExpression.isLocalized(this)
}

object Expression {
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
    * @param typeAnnotation The type that the variable declaration was annotated with.
    */
  case class VariableDeclaration(
    variable: LocalVariable,
    value: Expression,
    typeAnnotation: Option[Type],
    position: Position,
  ) extends Expression.Apply(TupleType.UnitType)

  case class Assignment(
    target: Expression.Access,
    value: Expression,
    position: Position,
  ) extends Expression.Apply(TupleType.UnitType)

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
  // Access expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A cross-cutting node trait signifying the possible access of a variable/property and thus the target
    * of an assignment.
    */
  sealed trait Access extends Expression {
    def label: String
    def isMutable: Boolean
    override def toString: String = label
  }

  case class BindingAccess(binding: TypedBinding, position: Position) extends Expression.Apply(binding.tpe) with Access {
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

    def integer(value: Long, position: Position): Literal = Literal(IntValue(value), position)
    def real(value: Double, position: Position): Literal = Literal(RealValue(value), position)
    def boolean(value: Boolean, position: Position): Literal = Literal(BooleanValue(value), position)
    def string(value: String, position: Position): Literal = Literal(StringValue(value), position)
  }

  case class Tuple(values: Vector[Expression], position: Position) extends Expression {
    override val tpe: TupleType = if (values.isEmpty) TupleType.UnitType else TupleType(values.map(_.tpe))
  }

  case class AnonymousFunction(
    parameters: Vector[AnonymousFunctionParameter],
    body: Expression,
    position: Position,
  ) extends Expression {
    /**
      * Whether the anonymous function only has annotated parameters. This allows the inference algorithm to infer the
      * type of the anonymous function directly.
      */
    lazy val isFullyAnnotated: Boolean = parameters.forall(_.isAnnotated)

    override val tpe: FunctionType = FunctionType(TupleType(parameters.map(_.tpe)), body.tpe)
  }

  case class AnonymousFunctionParameter(name: String, tpe: Type, position: Position) {
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

  case class Symbol(name: String, position: Position) extends Expression {
    override def tpe: SymbolType = SymbolType(name)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Cond(cases: Vector[CondCase], position: Position) extends Expression {
    /**
      * Whether the `cond` evaluates to a value in all cases. This is true if one case's condition is simply `true`,
      * which is the "else" branch of the cond.
      */
    val isTotal: Boolean = cases.exists(_.isTotalCase)

    override val tpe: Type = {
      // If there is no `true` case, we have to assume that the `cond` won't lead to a value in all instances. Hence,
      // we have to assume that Unit may be a result type.
      val bodyTypes = cases.map(_.body.tpe) ++ (if (!isTotal) Vector(TupleType.UnitType) else Vector.empty)
      SumType.construct(bodyTypes)
    }

    def withCases(pairs: Vector[(Expression, Expression)]): Cond = this.copy(pairs.map(CondCase.tupled))
  }

  case class CondCase(condition: Expression, body: Expression) {
    val isTotalCase: Boolean = condition match {
      case Literal(Literal.BooleanValue(true), _) => true
      case _ => false
    }
  }

  sealed trait Loop extends Expression {
    def body: Expression

    /**
      * A loop with element type Unit is treated as a special loop with type Unit.
      */
    def tpe: Type = body.tpe match {
      case TupleType.UnitType => TupleType.UnitType
      case elementType => ListType(elementType)
    }
  }

  case class WhileLoop(condition: Expression, body: Expression, position: Position) extends Loop

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
  case class Ascription(value: Expression, expectedType: Type, position: Position) extends Expression.Apply(expectedType)
}
