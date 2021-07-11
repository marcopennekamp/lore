package lore.compiler.phases.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}

object InstantiationTransformation {

  case class DuplicateProperty(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The property $name occurs more than once in the instantiation. Properties must be unique here."
  }

  case class MissingProperty(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"This map-style instantiation is missing a property $name."
  }

  case class IllegalProperty(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The struct to be instantiated does not have a property $name."
  }

  case class IllegallyTypedProperty(property: StructPropertyDefinition, expression: Expression) extends Feedback.Error(expression) {
    override def message: String =
      s"The property ${property.name} is supposed to be assigned a value of type ${expression.tpe}. However," +
        s" the property itself has the type ${property.tpe}, which is not a subtype of ${expression.tpe}."
  }

  def transformMapStyleInstantiation(struct: StructDefinition, entries: Vector[(String, Expression)], position: Position)(implicit reporter: Reporter): (Expression, Vector[TypingJudgment]) = {
    verifyNamesUnique(entries, position)
    val arguments = correlateEntries(struct, entries.toMap, position)
    val judgments = getEntryTypingJudgments(arguments)
    (Expression.Instantiation(struct, arguments, position), judgments)
  }

  private def verifyNamesUnique(entries: Vector[(String, Expression)], position: Position)(implicit reporter: Reporter): Unit = {
    entries.map(_._1).groupBy(identity).foreach {
      case (_, Vector(_)) =>
      case (name, _) => reporter.error(DuplicateProperty(name, position))
    }
  }

  /**
    * Assigns entries to properties, potentially filling missing properties with their default values.
    *
    * Missing properties without a default value and illegal properties are reported as errors.
    */
  private def correlateEntries(
    struct: StructDefinition,
    entries: Map[String, Expression],
    position: Position,
  )(implicit reporter: Reporter): Vector[Expression.Instantiation.Argument] = {
    var arguments = Vector.empty[Expression.Instantiation.Argument]
    var missing = Vector.empty[String]
    val illegal = entries.keys.toVector.diff(struct.properties.map(_.name))

    struct.properties.foreach { property =>
      entries.get(property.name) match {
        case Some(expression) =>
          arguments = arguments :+ Expression.Instantiation.Argument(property, expression)

        case None =>
          property.defaultValue match {
            case Some(defaultValue) =>
              val call = Expression.Call(defaultValue.callTarget, Vector.empty, defaultValue.tpe, position)
              val argument = Expression.Instantiation.Argument(property, call)
              arguments = arguments :+ argument

            case None => missing = missing :+ property.name
          }
      }
    }

    reporter.error(missing.map(MissingProperty(_, position)))
    reporter.error(illegal.map(IllegalProperty(_, position)))

    arguments
  }

  private def getEntryTypingJudgments(arguments: Vector[Expression.Instantiation.Argument]): Vector[TypingJudgment] = {
    arguments.map(argument => TypingJudgment.Subtypes(argument.value.tpe, argument.property.tpe, argument.value.position))
  }

}
