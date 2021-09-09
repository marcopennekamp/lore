package lore.compiler.transpilation

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.scopes.{StructObject, Variable}
import lore.compiler.semantics.structures.StructConstructor
import lore.compiler.target.Target.TargetExpression
import lore.compiler.target.TargetDsl.ExpressionExtension
import lore.compiler.target.{Target, TargetRepresentable}
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory

object TargetRepresentableTranspiler {

  /**
    * Transpiles a given target representable entity to its target expression.
    */
  def transpile(targetRepresentable: TargetRepresentable)(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    targetRepresentable match {
      case mf: MultiFunctionDefinition => RuntimeNames.multiFunction(mf)
      case function: FunctionDefinition => RuntimeNames.functionDefinition(function)
      case Variable(name, _, _) => RuntimeNames.localVariable(name)

      case structObject: StructObject =>
        val definition = structObject.tpe.schema.definition
        if (definition.allDefaultsLocalized) {
          RuntimeNames.struct.`object`(structObject.tpe.schema)
        } else {
          // Call the `value()` method of the LazyValue to get the actual object instance.
          RuntimeNames.struct.`object`(structObject.tpe.schema).prop("value").call()
        }

      case constructor: StructConstructor =>
        val schema = constructor.structType.schema
        if (schema.isConstant) {
          RuntimeNames.struct.constructor(schema)
        } else {
          val varSchema = RuntimeNames.schema(schema)
          val typeArguments = constructor.structType.typeArguments.map(TypeTranspiler.transpile)
          val varConstruct = RuntimeNames.struct.construct(schema)
          RuntimeApi.structs.getConstructor(varSchema, Target.List(typeArguments), varConstruct)
        }
    }
  }

}
