package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.types.TraitType

object TraitTranspiler {

  /**
    * Transpiles a trait type to its Javascript representation. To preserve symmetry with struct types, trait types
    * also have a schema.
    */
  def transpile(tpe: TraitType): Compilation[String] = {
    val (varSchema, schema) = transpileSchema(tpe)
    val varType = TranspiledName.declaredType(tpe)
    s"""$schema
       |const $varType = ${RuntimeApi.traits.tpe}($varSchema);
       |""".stripMargin.compiled
  }

  private def transpileSchema(tpe: TraitType) = {
    DeclaredTypeTranspiler.transpileSchema(
      tpe,
      RuntimeApi.traits.schema,
      Vector(
        // The trait's inherited shape type needs to be supplied to the runtime to support trait/shape subtyping.
        s"${RuntimeApi.utils.`lazy`.of}(() => ${RuntimeTypeTranspiler.transpile(tpe.inheritedShapeType)(Map.empty)})",
      )
    )
  }

}
