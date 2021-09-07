package lore.compiler.semantics.expressions

import lore.compiler.core.{CompilationException, Position, Positioned}
import lore.compiler.inference.InferenceVariable
import lore.compiler.semantics.functions.{CallTarget, FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.semantics.scopes.{TypedBinding, Variable}
import lore.compiler.types._

sealed trait Expression extends Positioned {
  def tpe: Type
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

  case class VariableDeclaration(
    variable: Variable,
    value: Expression,
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
  case class Block(expressions: Vector[Expression], position: Position) extends Expression {
    override val tpe: Type = expressions.lastOption.map(_.tpe).getOrElse(TupleType.UnitType)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Access expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A cross-cutting node trait signifying the possible access of a variable/property and thus the target
    * of an assignment.
    */
  sealed trait Access extends Expression {
    def name: String
    def isMutable: Boolean
  }

  case class BindingAccess(binding: TypedBinding, position: Position) extends Expression.Apply(binding.tpe) with Access {
    override val name: String = binding.name
    override val isMutable: Boolean = binding.isMutable
    override val toString: String = name
  }

  case class MemberAccess(instance: Expression, member: Member, position: Position) extends Expression.Apply(member.tpe) with Access {
    override val name: String = member.name
    override def isMutable: Boolean = member.isMutable
  }

  /**
    * A member access that cannot yet be resolved because the expression's type hasn't been inferred.
    */
  case class UnresolvedMemberAccess(instance: Expression, name: String, tpe: InferenceVariable, position: Position) extends Expression with Access {
    override def isMutable: Boolean = throw CompilationException(s"$this has an undefined mutability.")
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Literals and Value Constructors.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Literal(value: Any, tpe: BasicType, position: Position) extends Expression

  case class Tuple(values: Vector[Expression], position: Position) extends Expression {
    override val tpe: Type = if (values.isEmpty) TupleType.UnitType else TupleType(values.map(_.tpe))
  }

  case class AnonymousFunction(
    parameters: Vector[AnonymousFunctionParameter],
    body: Expression,
    position: Position,
  ) extends Expression {
    override val tpe: Type = FunctionType(TupleType(parameters.map(_.tpe)), body.tpe)
  }

  case class AnonymousFunctionParameter(name: String, tpe: Type, position: Position) {
    def mapType(f: Type => Type): AnonymousFunctionParameter = this.copy(tpe = f(tpe))
  }

  /**
    * A multi-function typed as a function. It can be passed around like any other function value.
    */
  case class MultiFunctionValue(mf: MultiFunctionDefinition, tpe: Type, position: Position) extends Expression

  /**
    * A fixed function instance typed as a function. It can be passed around like any other function value.
    */
  case class FixedFunctionValue(instance: FunctionInstance, position: Position) extends Expression {
    override def tpe: Type = instance.signature.functionType
  }

  case class ListConstruction(values: Vector[Expression], tpe: Type, position: Position) extends Expression

  case class MapConstruction(entries: Vector[MapEntry], tpe: Type, position: Position) extends Expression {
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
  case class IfElse(condition: Expression, onTrue: Expression, onFalse: Expression, tpe: Type, position: Position) extends Expression

  sealed trait Loop extends Expression {
    def body: Expression
  }

  case class WhileLoop(condition: Expression, body: Expression, tpe: Type, position: Position) extends Loop

  case class ForLoop(extractors: Vector[Extractor], body: Expression, tpe: Type, position: Position) extends Loop {
    /**
      * Creates a for-loop expression with the given collection values. The order of extractors and collections must be
      * compatible!
      */
    def withCollections(collections: Vector[Expression]): ForLoop = {
      this.copy(extractors.zip(collections).map { case (extractor, collection) => extractor.copy(collection = collection) })
    }
  }
  case class Extractor(variable: Variable, collection: Expression)
}
