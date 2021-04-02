package lore.compiler.semantics.expressions

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.phases.transformation.inference.InferenceVariable
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.semantics.members.Member
import lore.compiler.semantics.scopes.{LocalVariable, Variable}
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.types._

sealed trait Expression {
  def position: Position
  def tpe: Type
}

object Expression {
  abstract class Apply(override val tpe: Type) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Top-level expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Return(value: Expression, position: Position) extends Expression.Apply(BasicType.Nothing)

  case class VariableDeclaration(
    variable: LocalVariable, value: Expression, position: Position,
  ) extends Expression.Apply(ProductType.UnitType)

  case class Assignment(
    target: Expression.Access, value: Expression, position: Position,
  ) extends Expression.Apply(ProductType.UnitType)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Block(expressions: Vector[Expression], position: Position) extends Expression {
    override val tpe: Type = expressions.lastOption.map(_.tpe).getOrElse(ProductType.UnitType)
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

  case class VariableAccess(variable: Variable, position: Position) extends Expression.Apply(variable.tpe) with Access {
    override val name: String = variable.name
    override val isMutable: Boolean = variable.isMutable
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
    override val tpe: Type = if (values.isEmpty) ProductType.UnitType else ProductType(values.map(_.tpe))
  }

  case class AnonymousFunction(
    parameters: Vector[AnonymousFunctionParameter],
    body: Expression,
    position: Position,
  ) extends Expression {
    override val tpe: Type = FunctionType(ProductType(parameters.map(_.tpe)), body.tpe)
  }

  case class AnonymousFunctionParameter(name: String, tpe: Type, position: Position) {
    def mapType(f: Type => Type): AnonymousFunctionParameter = this.copy(tpe = f(tpe))
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

  /**
    * Creates a new instance of a given struct. This expression represents both the call and map syntax. The arguments
    * are passed in their syntactic order to preserve the intended execution order of side effects and should thus be
    * transpiled such that their evaluation results in the same order.
    */
  case class Instantiation(struct: StructDefinition, arguments: Vector[Instantiation.Argument], position: Position) extends Expression {
    override def tpe: Type = struct.tpe

    /**
      * Creates a new instantiation expression with the given argument values. The order of arguments and values must
      * be compatible!
      */
    def withArgumentValues(values: Vector[Expression]): Instantiation = {
      this.copy(arguments = arguments.zip(values).map { case (argument, value) => argument.copy(value = value) })
    }
  }
  object Instantiation {
    case class Argument(property: StructPropertyDefinition, value: Expression)
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

  trait Loop extends Expression {
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
  case class Extractor(variable: LocalVariable, collection: Expression)
}
