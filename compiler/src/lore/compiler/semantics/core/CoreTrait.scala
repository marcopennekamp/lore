package lore.compiler.semantics.core

import lore.compiler.semantics.NamePath
import lore.compiler.types.{BasicType, TraitSchema, Type, TypeSchema}

/**
  * A core trait is a trait that the compiler might directly depend on. For example, type introspection works with a
  * `Type` trait that represents run-time types.
  *
  * @param schema The underlying schema. `None` if it cannot be resolved.
  */
class CoreTrait(val name: NamePath, val schema: Option[TraitSchema]) {
  val schemaOrNothing: TypeSchema = schema.getOrElse(BasicType.Nothing)
  val schemaOrAny: TypeSchema = schema.getOrElse(BasicType.Any)
  val typeOrNothing: Type = schema.map(_.representative).getOrElse(BasicType.Nothing)
  val typeOrAny: Type = schema.map(_.representative).getOrElse(BasicType.Any)
}
