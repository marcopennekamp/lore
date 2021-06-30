package lore.compiler.target

/**
  * An intermediate representation of the target language the compiler generates code for. The main purpose of this
  * representation is to remove the burden of working with strings during the transpilation phase.
  *
  * In the long-term, such an intermediate representation may be used to support multiple target languages. The
  * representation is chosen in such a way as to keep it as decoupled from JS as possible.
  */
object Target {
  sealed trait TargetStatement
  sealed trait TargetExpression extends TargetStatement

  class TargetName(val name: String) extends AnyVal {
    def asVariable: Variable = Variable(this)
    def asParameter: Parameter = Parameter(this)

    override def toString: String = name
  }

  case object Empty extends TargetStatement
  case object Divider extends TargetStatement

  // Control Structures.
  case class Block(statements: Vector[TargetStatement]) extends TargetStatement
  case class IfElse(condition: TargetExpression, thenStatement: TargetStatement, elseStatement: TargetStatement) extends TargetStatement
  case class While(condition: TargetExpression, body: TargetStatement) extends TargetStatement
  case class For(init: TargetStatement, condition: TargetExpression, post: TargetStatement, body: TargetStatement) extends TargetStatement
  case class Iteration(collection: TargetExpression, elementName: TargetName, body: TargetStatement) extends TargetStatement
  case class Return(value: TargetExpression) extends TargetStatement

  def block(statements: TargetStatement*): Block = Block(statements.toVector)

  // Variables.
  case class VariableDeclaration(name: TargetName, value: TargetExpression, isMutable: Boolean = false) extends TargetStatement
  case class Assignment(left: TargetExpression, right: TargetExpression) extends TargetStatement
  case class Variable(name: TargetName) extends TargetExpression {
    lazy val asParameter: Parameter = Parameter(name)
  }

  // Functions.
  case class Function(name: TargetName, parameters: Vector[Parameter], body: Block, shouldExport: Boolean = false) extends TargetStatement
  case class Lambda(parameters: Vector[Parameter], body: TargetStatement) extends TargetExpression
  case class Parameter(name: TargetName, default: Option[TargetExpression] = None, isRestParameter: Boolean = false) {
    lazy val asVariable: Variable = Variable(name)
  }

  /**
    * @param isRestCall The last argument is generated as a rest parameter spread.
    */
  case class Call(function: TargetExpression, arguments: Vector[TargetExpression], isRestCall: Boolean = false) extends TargetExpression
  case class New(constructor: TargetExpression, arguments: Vector[TargetExpression]) extends TargetExpression

  // Values.
  case class RealLiteral(value: Double) extends TargetExpression
  case class IntLiteral(value: Long) extends TargetExpression
  case class BooleanLiteral(value: Boolean) extends TargetExpression
  case class StringLiteral(value: String) extends TargetExpression
  case object Undefined extends TargetExpression
  case object Null extends TargetExpression

  case class Dictionary(properties: Vector[Property]) extends TargetExpression
  case class Property(name: TargetName, value: TargetExpression)

  case class List(elements: Vector[TargetExpression]) extends TargetExpression

  // Operations.
  case class Operation(operator: TargetOperator, operands: Vector[TargetExpression]) extends TargetExpression
  case class PropertyAccess(instance: TargetExpression, name: TargetName) extends TargetExpression
  case class ListAccess(list: TargetExpression, key: TargetExpression) extends TargetExpression

  /**
    * Whether the given statement has no semantic significance at all.
    */
  def isEmpty(statement: TargetStatement): Boolean = statement match {
    case Target.Empty => true
    case Target.Block(statements) => statements.isEmpty
    case _ => false
  }
}
