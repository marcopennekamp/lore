package lore.compiler.phases.transpilation.structures

import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeNames, TypeTranspiler}
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetName}
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.StructType

object InstantiationTranspiler {

  type PropertyAssignment = (TargetName, TargetExpression)

  /**
    * Creates a struct instantiation, using the generated [[RuntimeNames.struct.instantiate]] function. The
    * `instantiate` function expects a properties object and a type arguments list (if the schema isn't constant) as
    * parameters.
    */
  def transpileStructInstantiation(
    structType: StructType,
    propertyAssignments: Vector[PropertyAssignment],
  )(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    println(s"Direct `instantiate` call of $structType!")

    val properties = Target.Dictionary(
      propertyAssignments.map { case (name, value) => Target.Property(name, value) }
    )
    val typeArguments = if (!structType.schema.isConstant) {
      Some(Target.List(structType.typeArguments.map(TypeTranspiler.transpile)))
    } else None

    val varInstantiate = RuntimeNames.struct.instantiate(structType.schema)
    Target.Call(varInstantiate, Vector(properties) ++ typeArguments.toVector)
  }

}
