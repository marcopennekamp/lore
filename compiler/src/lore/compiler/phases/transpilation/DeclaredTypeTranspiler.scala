package lore.compiler.phases.transpilation

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.types.{DeclaredType, StructType, TraitType}

object DeclaredTypeTranspiler {

  /**
    * Transpiles the given declared type and definition to its representation. Any types this type depends on must be
    * transpiled before this type is transpiled!
    */
  def transpile(tpe: DeclaredType): Compilation[String] = {
    tpe match {
      case structType: StructType => StructTranspiler.transpile(structType)
      case traitType: TraitType => TraitTranspiler.transpile(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

}
