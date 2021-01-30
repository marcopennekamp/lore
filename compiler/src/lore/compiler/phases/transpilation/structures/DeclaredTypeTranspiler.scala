package lore.compiler.phases.transpilation.structures

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.Registry
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.types.{DeclaredType, StructType, TraitType}

object DeclaredTypeTranspiler {

  /**
    * Transpiles the given declared type and definition to its representation. Any types this type depends on (eagerly)
    * must be transpiled before this type is transpiled!
    */
  def transpile(tpe: DeclaredType)(implicit registry: Registry): Vector[TargetStatement] = {
    tpe match {
      case structType: StructType => StructTranspiler.transpile(structType)
      case traitType: TraitType => TraitTranspiler.transpile(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  def transpileSupertraits(tpe: DeclaredType): Vector[TargetExpression] = tpe.declaredSupertypes.map(RuntimeNames.declaredType(_).asVariable)

}
