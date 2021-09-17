package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter, StructFeedback}
import lore.compiler.inference.InferenceVariable
import lore.compiler.resolution.TypeExpressionEvaluator
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.{BindingScope, StructObjectBinding, StructConstructorBinding, TypeScope}
import lore.compiler.semantics.structures.{StructConstructor, StructDefinition, StructPropertyDefinition}
import lore.compiler.syntax.TypeExprNode

object StructTransformation {

  /**
    * Gets the struct binding called `name` from the given scope.
    */
  def getStructConstructorBinding(name: String, position: Position)(implicit bindingScope: BindingScope, reporter: Reporter): Option[StructConstructorBinding] = {
    bindingScope.resolve(name, position).flatMap {
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
    * Gets a struct constructor from the given struct constructor binding. If the struct has type parameters, all type
    * parameters in the constructor's type are replaced with inference variables. Type parameter bounds are added as
    * typing judgments.
    *
    * This function should be used if there are no manually specified type arguments.
    */
  def getConstructor(structConstructorBinding: StructConstructorBinding, position: Position)(implicit judgmentCollector: JudgmentCollector): StructConstructor = {
    if (structConstructorBinding.isConstant) {
      structConstructorBinding.instantiate(Map.empty)
    } else {
      val (assignments, judgments) = InferenceVariable.fromTypeVariables(structConstructorBinding.parameters, position)
      judgmentCollector.add(judgments)
      structConstructorBinding.instantiate(assignments)
    }
  }

  /**
    * Gets a constructor from the struct binding identified by `name`, instantiating the struct schema with the given
    * type arguments (which may not contain any inference variables). If no type arguments are supplied, the
    * constructor type is inferred.
    */
  def getConstructor(
    name: String,
    typeArgumentNodes: Option[Vector[TypeExprNode]],
    position: Position,
  )(implicit bindingScope: BindingScope, typeScope: TypeScope, judgmentCollector: JudgmentCollector, reporter: Reporter): Option[StructConstructor] = {
    StructTransformation.getStructConstructorBinding(name, position).map { structBinding =>
      typeArgumentNodes match {
        case Some(nodes) => structBinding.asSchema.instantiate(nodes.map(TypeExpressionEvaluator.evaluate), position).constructor
        case None => getConstructor(structBinding, position)
      }
    }
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
