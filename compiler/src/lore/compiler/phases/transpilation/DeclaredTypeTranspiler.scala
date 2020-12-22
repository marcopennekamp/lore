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
    val schema =
      s"""const $varSchema = $schemaFunction(
         |  '${tpe.name}',
         |  [${varDeclaredSupertypes.mkString(", ")}],
         |  ${additionalArguments.mkString(", ")}
         |);""".stripMargin
    (varSchema, schema)
  }

}
