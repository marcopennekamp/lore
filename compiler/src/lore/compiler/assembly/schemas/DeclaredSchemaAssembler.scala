package lore.compiler.assembly.schemas

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemFunction, PoemGlobalVariable, PoemNamedType, PoemSchema}
import lore.compiler.semantics.Registry
import lore.compiler.types.{DeclaredSchema, StructSchema, TraitSchema}

object DeclaredSchemaAssembler {

  /**
    * Generates a PoemSchema for the given schema, and any auxiliary functions the schema requires, such as a
    * constructor.
    */
  def generate(schema: DeclaredSchema)(implicit registry: Registry): (PoemSchema, Vector[PoemFunction], Option[PoemGlobalVariable]) = {
    val poemTypeParameters = schema.parameters.map(TypeAssembler.generateParameter)
    val poemSupertraits = schema.declaredSupertypes.map(TypeAssembler.generate).asInstanceOf[Vector[PoemNamedType]]

    schema match {
      case schema: TraitSchema => (TraitSchemaAssembler.generate(schema, poemTypeParameters, poemSupertraits), Vector.empty, None)
      case schema: StructSchema => StructSchemaAssembler.generate(schema, poemTypeParameters, poemSupertraits)
    }
  }

}
