package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.types.{DeclaredType, StructType, TraitType}

object DeclaredTypeTranspiler {

  /**
    * Transpiles the given declared type to its representation. Any types this type depends on must be transpiled
    * before this type is transpiled!
    */
  def transpile(tpe: DeclaredType): Compilation[String] = {
    tpe match {
      case structType: StructType => transpileStructType(structType)
      case traitType: TraitType => transpileTraitType(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  private def transpileStructType(tpe: StructType): Compilation[String] = {
    val varStructType = TranspiledNames.namedType(tpe)
    val varDeclaredSupertypes = tpe.declaredSupertypes.map(TranspiledNames.namedType).mkString(", ")
    // TODO: Support componentTypes.
    // TODO: Support ownedBy types.
    // TODO: Support isEntity.
    s"""const $varStructType = ${RuntimeApi.types.structType}(
       |  '${tpe.name}',
       |  $varDeclaredSupertypes,
       |  [],
       |  undefined,
       |  false,
       |)""".stripMargin.compiled
  }

  private def transpileTraitType(tpe: TraitType): Compilation[String] = {
    "".compiled // TODO: Implement.
  }

}
