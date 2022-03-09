package lore.compiler.assembly.schemas

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemNamedType, PoemShapeType, PoemTraitSchema, PoemTypeParameter}
import lore.compiler.types.{Kind, TraitSchema}

object TraitSchemaAssembler {

  def generate(
    schema: TraitSchema,
    poemTypeParameters: Vector[PoemTypeParameter],
    poemSupertraits: Vector[PoemNamedType],
  ): PoemTraitSchema = {
    val poemInheritedShapeType = TypeAssembler.generate(schema.inheritedShapeType).asInstanceOf[PoemShapeType]
    PoemTraitSchema(Kind.Trait, schema.name, poemTypeParameters, poemSupertraits, poemInheritedShapeType)
  }

}
