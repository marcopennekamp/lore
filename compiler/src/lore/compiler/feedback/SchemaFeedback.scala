package lore.compiler.feedback

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.types.{DeclaredSchema, TraitSchema, Type, TypeVariable}

object SchemaFeedback {

  case class UndefinedDependency(
    tpe: TypeDefinition,
    dependency: NamePath,
  ) extends Feedback.Error(tpe) {
    override def message: String = s"The type `${tpe.name}` depends on a type `$dependency`, but it doesn't exist."
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(
    typeNames: Vector[NamePath],
    occurrence: TypeDefinition,
  ) extends Feedback.Error(occurrence) {
    override def message: String = s"An inheritance cycle between the following types has been detected:" +
      s" ${typeNames.mkString(", ")}. A declared type must not inherit from itself directly or indirectly. The" +
      s" subtyping relationships of declared types must result in a directed, acyclic graph."
  }

  case class IllegalExtends(schema: DeclaredSchema, supertypeName: Option[NamePath]) extends Feedback.Error(schema) {
    override def message: String = supertypeName match {
      case Some(name) => s"The trait or struct `${schema.name}` extends a type `$name` which is not a trait or shape."
      case None => s"The trait or struct `${schema.name}` extends a type other than a trait or shape."
    }
  }

  case class SupertraitInvarianceInconsistent(
    schema: DeclaredSchema,
    supertraitSchema: TraitSchema,
    typeParameter: TypeVariable,
    typeArguments: Vector[Type],
  ) extends Feedback.Error(schema) {
    override def message: String = s"The invariant parameter `${typeParameter.name}` of trait" +
      s" `${supertraitSchema.name}`, which `${schema.name}` inherits from, has multiple conflicting type arguments:" +
      s" ${typeArguments.mkString(", ")}."
  }

}
