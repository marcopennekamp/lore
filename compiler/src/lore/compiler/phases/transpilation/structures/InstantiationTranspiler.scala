package lore.compiler.phases.transpilation.structures

import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeNames, TypeTranspiler}
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetName}
import lore.compiler.types.StructType

object InstantiationTranspiler {

  type PropertyAssignment = (TargetName, TargetExpression)

  /**
    * Creates a struct instantiation, using the generated [[RuntimeNames.struct.construct]] function. The `construct`
    * function expects a type arguments list (if the schema is parametric) and the property arguments as parameters.
    */
  def transpileStructInstantiation(
    structType: StructType,
    arguments: Vector[TargetExpression],
  )(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    println(s"Direct `construct` call of $structType!")

    val typeArguments = if (!structType.schema.isConstant) {
      Vector(Target.List(structType.typeArguments.map(TypeTranspiler.transpile)))
    } else Vector.empty

    val varConstruct = RuntimeNames.struct.construct(structType.schema)
    Target.Call(varConstruct, typeArguments ++ arguments)
  }

}
