package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.types.{ClassType, LabelType, NamedType}

object NamedTypeTranspiler {
  /**
    * Transpiles the given named type to its representation. Any types this type depends on must be transpiled
    * before this type is transpiled!
    */
  def transpile(tpe: NamedType): Compilation[String] = {
    tpe match {
      case classType: ClassType => transpileClassType(classType)
      case labelType: LabelType => transpileLabelType(labelType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  private def transpileClassType(classType: ClassType): Compilation[String] = {
    val varClassType = TranspiledNames.namedType(classType)
    val varSupertype = classType.supertype.map(TranspiledNames.namedType).getOrElse("undefined")
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

  private def transpileLabelType(labelType: LabelType): Compilation[String] = {
    "".compiled // TODO: Implement.
  }
}
