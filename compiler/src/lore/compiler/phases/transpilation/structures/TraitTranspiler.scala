package lore.compiler.phases.transpilation.structures

import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeApi, RuntimeNames, TypeTranspiler}
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl._
import lore.compiler.types.TraitType

object TraitTranspiler {

  /**
    * Transpiles a trait type to its target representation.
    */
  def transpile(tpe: TraitType)(implicit symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    val schema = RuntimeApi.traits.schema(
      tpe.name,
      DeclaredTypeTranspiler.transpileSupertraits(tpe),
      // The trait's inherited shape type needs to be supplied to the runtime to support trait/shape subtyping.
      RuntimeApi.utils.`lazy`.of(TypeTranspiler.transpile(tpe.inheritedShapeType)(Map.empty, symbolHistory)),
    )

    val varSchema = RuntimeNames.declaredSchema(tpe.schema)
    val varType = RuntimeNames.declaredType(tpe)
    Vector(
      varSchema.declareAs(schema),
      varType.declareAs(RuntimeApi.traits.tpe(varSchema)),
    )
  }

}
