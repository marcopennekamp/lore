package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.types.{StructType, TraitType, NamedType}

object NamedTypeTranspiler {
  /**
    * Transpiles the given named type to its representation. Any types this type depends on must be transpiled
    * before this type is transpiled!
    */
  def transpile(tpe: NamedType): Compilation[String] = {
    tpe match {
      case classType: StructType => transpileClassType(classType)
      case labelType: TraitType => transpileLabelType(labelType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  private def transpileClassType(classType: StructType): Compilation[String] = {
    val varClassType = TranspiledNames.namedType(classType)
    val varSupertype = classType.supertypes.map(TranspiledNames.namedType).getOrElse("undefined")
    // TODO: Support ownedBy types.
    // TODO: Support componentTypes and isEntity.
    s"""const $varClassType = ${RuntimeApi.types.classType}(
       |  '${classType.name}',
       |  $varSupertype,
       |  undefined,
       |  [],
       |  false,
       |)""".stripMargin.compiled
  }

  private def transpileLabelType(labelType: TraitType): Compilation[String] = {
    "".compiled // TODO: Implement.
  }
}
