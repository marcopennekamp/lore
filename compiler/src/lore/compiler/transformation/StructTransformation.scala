package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter, StructFeedback}
import lore.compiler.resolution.TypeExpressionEvaluator
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.{BindingScope, StructConstructorBinding, StructObjectBinding, TypeScope}
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.syntax.TypeExprNode

object StructTransformation {

  /**
    * Gets the constructor binding corresponding to the given struct `name`.
    */
  def getConstructorBinding(
    name: NamePath,
    position: Position,
  )(implicit bindingScope: BindingScope, reporter: Reporter): Option[StructConstructorBinding] = {
    bindingScope.resolveStatic(name, position).flatMap {
      case binding: StructConstructorBinding => Some(binding)

      case _: StructObjectBinding =>
        reporter.error(StructFeedback.Object.NoConstructor(name, position))
        None

      case _ =>
        reporter.error(StructFeedback.ConstructorExpected(name, position))
        None
    }
  }

  /**
    * Instantiates the given struct constructor binding with the given type arguments.
    */
  def getConstructorValue(
    binding: StructConstructorBinding,
    typeArgumentNodes: Vector[TypeExprNode],
    position: Position,
  )(implicit bindingScope: BindingScope, typeScope: TypeScope, reporter: Reporter): Expression.ConstructorValue = {
    val typeArguments = typeArgumentNodes.map(TypeExpressionEvaluator.evaluate)
    val structType = binding.instantiateStructType(typeArguments, position)
    Expression.ConstructorValue(binding, structType, position)
  }

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

  /**
    * Transforms the name/expression pairs in `entries` to an ordered list of arguments with which the struct's
    * constructor may be invoked.
    */
  def entriesToArguments(struct: StructDefinition, entries: Vector[(String, Expression)], position: Position)(implicit reporter: Reporter): Vector[Expression] = {
    verifyNamesUnique(entries, position)
    correlateEntries(struct, entries.toMap, position)
  }

  private def verifyNamesUnique(entries: Vector[(String, Expression)], position: Position)(implicit reporter: Reporter): Unit = {
    entries.map(_._1).groupBy(identity).foreach {
      case (_, Vector(_)) =>
      case (name, _) => reporter.error(DuplicateProperty(name, position))
    }
  }

  /**
    * Assigns entries to properties, potentially filling missing properties with their default values. Missing
    * properties without a default value and illegal properties are reported as errors. The result is a list of struct
    * constructor arguments in the correct order.
    */
  private def correlateEntries(
    struct: StructDefinition,
    entries: Map[String, Expression],
    position: Position,
  )(implicit reporter: Reporter): Vector[Expression] = {
    var arguments = Vector.empty[Expression]
    var missing = Vector.empty[String]
    val illegal = entries.keys.toVector.diff(struct.properties.map(_.name))

    struct.properties.foreach { property =>
      entries.get(property.name) match {
        case Some(expression) =>
          arguments = arguments :+ expression

        case None =>
          property.defaultValue match {
            case Some(defaultValue) =>
              // TODO (assembly): This needs to be changed to a PropertyDefaultValue expression, because this being a
              //                  call is an assembly detail.
              val expression = Expression.Call(defaultValue.callTarget, Vector.empty, defaultValue.tpe, position)
              arguments = arguments :+ expression

            case None => missing = missing :+ property.name
          }
      }
    }

    reporter.error(missing.map(MissingProperty(_, position)))
    reporter.error(illegal.map(IllegalProperty(_, position)))

    arguments
  }

}
