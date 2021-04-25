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

  /**
    * Transpiles any additional definitions of the given declared type that could depend on ANY type regardless of type
    * order. These have to be initialized after all types have been initialized, hence the "deferred" transpilation.
    */
  def transpileDeferred(tpe: DeclaredType)(implicit registry: Registry): Vector[TargetStatement] = {
    tpe match {
      case structType: StructType => Vector(StructTranspiler.transpileConstructor(structType))
      case _ => Vector.empty
    }
  }

  def transpileSupertraits(tpe: DeclaredType): Vector[TargetExpression] = tpe.declaredSupertypes.map(RuntimeNames.declaredType)

}
