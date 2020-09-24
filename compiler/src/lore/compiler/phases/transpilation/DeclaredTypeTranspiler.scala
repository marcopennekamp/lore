package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
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
      case traitType: TraitType => transpileTraitType(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  /**
    * Transpiles a trait type to its Javascript representation.
    *
    * To preserve symmetry with struct types, trait types also have a schema and a newtype function. These are not
    * strictly necessary, but ease the burden on runtime definitions.
    */
  private def transpileTraitType(tpe: TraitType): Compilation[String] = {
    // TODO: Implement.
    val varTraitType = TranspiledNames.declaredType(tpe)
    s"""const $varTraitType = { };
       |""".stripMargin.compiled
  }

}
