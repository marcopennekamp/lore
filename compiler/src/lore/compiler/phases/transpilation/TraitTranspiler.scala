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
    val (varSchema, schema) = DeclaredTypeTranspiler.transpileSchema(tpe, RuntimeApi.types.schema.`trait`)

    val varType = TranspiledName.declaredType(tpe)
    s"""$schema
       |const $varType = ${RuntimeApi.types.`trait`}($varSchema);
       |""".stripMargin.compiled
  }

}
