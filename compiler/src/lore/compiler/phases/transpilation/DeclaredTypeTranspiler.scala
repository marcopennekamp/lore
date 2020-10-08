package lore.compiler.phases.transpilation

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.semantics.Registry
import lore.compiler.types.{DeclaredType, StructType, TraitType}

object DeclaredTypeTranspiler {

  /**
    * Transpiles the given declared type and definition to its representation. Any types this type depends on must be
    * transpiled before this type is transpiled!
    */
  def transpile(tpe: DeclaredType)(implicit registry: Registry): Compilation[String] = {
    tpe match {
      case structType: StructType => StructTranspiler.transpile(structType)
      case traitType: TraitType => TraitTranspiler.transpile(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  /**
    * Transpiles the schema creation of the declared type, returning the schema variable and schema code.
    */
  def transpileSchema(
    tpe: DeclaredType, schemaFunction: String, additionalArguments: Vector[String] = Vector.empty
  ): (TranspiledName, String) = {
    val varSchema = TranspiledName.typeSchema(tpe)
    val varDeclaredSupertypes = tpe.declaredSupertypes.map(TranspiledName.declaredType)
    // TODO: Don't we have to take care that owned-by types are ordered? Otherwise, we might have an owned-by type
    //       A in a schema, but A is undefined at that point and only later defined. Do we have to use lazy loading
    //       here? Or add owned-by types to the DeclarationResolver?
    val ownedBy = RuntimeTypeTranspiler.transpile(tpe.ownedBy)(Map.empty)
    val schema =
      s"""const $varSchema = $schemaFunction(
         |  '${tpe.name}',
         |  [${varDeclaredSupertypes.mkString(", ")}],
         |  ${RuntimeApi.utils.`lazy`.of}(() => $ownedBy),
         |  ${tpe.isEntity},
         |  ${additionalArguments.mkString(", ")}
         |);""".stripMargin
    (varSchema, schema)
  }

}
