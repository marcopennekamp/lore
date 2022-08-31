package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{Reporter, StructFeedback}
import lore.compiler.resolution.TypeResolver
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.{StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedConstructorValue, UntypedPropertyDefaultValue}
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types.StructSchema

object StructTransformation {

  /**
    * Gets the constructor binding corresponding to the given struct `name`.
    */
  def getConstructorBinding(
    name: NamePath,
    position: Position,
  )(implicit termScope: TermScope, reporter: Reporter): Option[StructConstructorBinding] = {
    termScope.resolveStatic(name, position).flatMap {
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
  )(implicit termScope: TermScope, typeScope: TypeScope, reporter: Reporter): UntypedConstructorValue = {
    val typeArguments = typeArgumentNodes.map(TypeResolver.resolve)
    val structType = binding.instantiateStructType(typeArguments, position)
    UntypedConstructorValue(structType, position)
  }

  /**
    * Transforms the name/expression pairs in `entries` to an ordered list of arguments with which the struct's
    * constructor may be invoked.
    */
  def entriesToArguments(
    struct: StructSchema,
    entries: Vector[(String, UntypedExpression)],
    position: Position,
  )(implicit reporter: Reporter): Vector[UntypedExpression] = {
    verifyNamesUnique(entries, position)
    correlateEntries(struct, entries.toMap, position)
  }

  private def verifyNamesUnique(
    entries: Vector[(String, UntypedExpression)],
    position: Position,
  )(implicit reporter: Reporter): Unit = {
    entries.map(_._1).groupBy(identity).foreach {
      case (_, Vector(_)) =>
      case (name, _) => reporter.error(StructFeedback.Instantiation.DuplicateProperty(name, position))
    }
  }

  /**
    * Assigns entries to properties, potentially filling missing properties with their default values. Missing
    * properties without a default value and illegal properties are reported as errors. The result is a list of struct
    * constructor arguments in the correct order.
    */
  private def correlateEntries(
    struct: StructSchema,
    entries: Map[String, UntypedExpression],
    position: Position,
  )(implicit reporter: Reporter): Vector[UntypedExpression] = {
    var arguments = Vector.empty[UntypedExpression]
    var missing = Vector.empty[String]
    val illegal = entries.keys.toVector.diff(struct.properties.map(_.name))

    struct.properties.foreach { property =>
      entries.get(property.name) match {
        case Some(expression) => arguments = arguments :+ expression
        case None if property.hasDefault => arguments = arguments :+ UntypedPropertyDefaultValue(property, position)
        case None => missing = missing :+ property.name
      }
    }

    reporter.error(missing.map(StructFeedback.Instantiation.MissingProperty(_, position)))
    reporter.error(illegal.map(StructFeedback.Instantiation.IllegalProperty(_, position)))

    arguments
  }

}
