package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.structures.{StructPropertyDefinition, StructDefinition}

object InstantiationTransformations {

  def transformCallStyleInstantiation(struct: StructDefinition, arguments: Vector[Expression])(implicit position: Position): Compilation[Expression] = {
    ExpressionVerification.adhereToSignature(arguments, struct.constructorSignature, position).map { _ =>
      val instantiationArguments = struct.properties.zip(arguments).map(Expression.Instantiation.Argument.tupled)
      Expression.Instantiation(struct, instantiationArguments, position)
    }
  }

  case class DuplicateProperty(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The property $name occurs more than once in the instantiation. Properties must be unique here."
  }

  case class MissingProperty(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"This map-style instantiation is missing a property $name."
  }

  case class IllegalProperty(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The struct to be instantiated does not have a property $name."
  }

  case class IllegallyTypedProperty(property: StructPropertyDefinition, expression: Expression) extends Error(expression) {
    override def message: String =
      s"The property ${property.name} is supposed to be assigned a value of type ${expression.tpe}. However," +
        s" the property itself has the type ${property.tpe}, which is not a subtype of ${expression.tpe}."
  }

  def transformMapStyleInstantiation(struct: StructDefinition, entries: Vector[(String, Expression)])(implicit position: Position): Compilation[Expression] = {
    def verifyNamesUnique(): Verification = {
      entries.map(_._1).groupBy(identity).map {
        case (name, vector) if vector.length > 1 => Compilation.fail(DuplicateProperty(name))
        case _ => Verification.succeed
      }.toVector.simultaneous.verification
    }

    /**
      * Assigns entries to properties, potentially filling missing properties with their default values.
      */
    def correlateEntries(): Compilation[Vector[(StructPropertyDefinition, Expression)]] = {
      var pairs = Vector.empty[(StructPropertyDefinition, Expression)]
      var missing = Vector.empty[String]
      val illegal = entries.map(_._1).diff(struct.properties.map(_.name))

      struct.properties.foreach { property =>
        entries.find { case (name, _) => name == property.name } match {
          case Some((_, expression)) =>
            pairs = pairs :+ (property, expression)
          case None =>
            property.defaultValue match {
              case Some(defaultValue) => pairs = pairs :+ (property, Expression.Call(defaultValue.callTarget, Vector.empty, position))
              case None => missing = missing :+ property.name
            }
        }
      }

      if (missing.isEmpty && illegal.isEmpty) {
        Compilation.succeed(pairs)
      } else {
        Compilation.fail(missing.map(MissingProperty(_)) ++ illegal.map(IllegalProperty(_)): _*)
      }
    }

    def verifyEntryTypes(pairs: Vector[(StructPropertyDefinition, Expression)]): Verification = {
      pairs.map { case (property, expression) =>
        if (expression.tpe <= property.tpe) Verification.succeed else Compilation.fail(IllegallyTypedProperty(property, expression))
      }.simultaneous.verification
    }

    for {
      _ <- verifyNamesUnique()
      pairs <- correlateEntries()
      _ <- verifyEntryTypes(pairs)
      arguments = pairs.map(Expression.Instantiation.Argument.tupled)
    } yield Expression.Instantiation(struct, arguments, position)
  }

}
