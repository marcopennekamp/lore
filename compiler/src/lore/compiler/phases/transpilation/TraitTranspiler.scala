package lore.compiler.phases.transpilation

import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.VariableExtension
import lore.compiler.types.TraitType

object TraitTranspiler {

  /**
    * Transpiles a trait type to its target representation.
    */
  def transpile(tpe: TraitType): Vector[TargetStatement] = {
    val schema = RuntimeApi.traits.schema(
      tpe.name,
      DeclaredTypeTranspiler.transpileSupertraits(tpe),
      // The trait's inherited shape type needs to be supplied to the runtime to support trait/shape subtyping.
      RuntimeApi.utils.`lazy`.of(RuntimeTypeTranspiler.transpile(tpe.inheritedShapeType)(Map.empty))
    )

    val varSchema = TranspiledName.typeSchema(tpe).asVariable
    val varType = TranspiledName.declaredType(tpe).asVariable
    Vector(
      varSchema.declareAs(schema),
      varType.declareAs(RuntimeApi.traits.tpe(varSchema)),
    )
  }

}
