package lore.compiler.types

case class TraitType(
  schema: TraitSchema,
  assignments: TypeVariable.Assignments,
) extends DeclaredType
