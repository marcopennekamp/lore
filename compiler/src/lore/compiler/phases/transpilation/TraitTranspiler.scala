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
    // TODO: Can't we solve most of the schema generation in a shared DeclaredTypeTranspiler method?
    val varSchema = TranspiledNames.typeSchema(tpe)
    val varDeclaredSupertypes = tpe.declaredSupertypes.map(TranspiledNames.declaredType)
    val inheritedComponentTypes = tpe.inheritedComponentTypes.map(RuntimeTypeTranspiler.transpile(_)(Map.empty))
    // TODO: The same applies here about owned-by types as in StructTranspiler.
    val ownedBy = RuntimeTypeTranspiler.transpile(tpe.ownedBy)(Map.empty)
    val schema =
      s"""const $varSchema = ${RuntimeApi.types.schema.`trait`}(
         |  '${tpe.name}',
         |  [${varDeclaredSupertypes.mkString(", ")}],
         |  [${inheritedComponentTypes.mkString(", ")}],
         |  $ownedBy,
         |  ${tpe.isEntity},
         |);""".stripMargin

    val varType = TranspiledNames.declaredType(tpe)
    s"""$schema
       |const $varType = ${RuntimeApi.types.`trait`}($varSchema);
       |""".stripMargin.compiled
  }

}
