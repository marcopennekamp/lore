package lore.compiler.types

case class TraitType(
  schema: TraitSchema,
  typeArguments: TypeVariable.Assignments,
) extends DeclaredType
