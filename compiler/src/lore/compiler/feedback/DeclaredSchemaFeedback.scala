package lore.compiler.feedback

import lore.compiler.types.{DeclaredSchema, TraitSchema, Type, TypeVariable}

object DeclaredSchemaFeedback {

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
