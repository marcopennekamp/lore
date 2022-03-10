package lore.compiler.assembly.functions

import lore.compiler.assembly.{AsmChunk, AsmRuntimeNames, RegisterProvider}
import lore.compiler.poem.PoemInstruction
import lore.compiler.semantics.scopes.{LocalVariable, StructObjectBinding, TypedBinding}
import lore.compiler.semantics.variables.GlobalVariableDefinition

object TypedBindingAssembler {

  def generate(binding: TypedBinding)(
    implicit registerProvider: RegisterProvider,
    variableRegisterMap: VariableRegisterMap,
    capturedVariableMap: CapturedVariableMap,
  ): AsmChunk = {
    binding match {
      case global: GlobalVariableDefinition =>
        val target = registerProvider.fresh()
        AsmChunk(target, PoemInstruction.GlobalGet(target, global.name))

      case variable: LocalVariable =>
        // If the variable is a captured variable, we need to load its value from the lambda context via `LambdaLocal`.
        capturedVariableMap.get(variable.uniqueKey) match {
          case Some(index) =>
            val target = registerProvider.fresh()
            AsmChunk(target, PoemInstruction.LambdaLocal(target, index))

          case None =>
            AsmChunk(variableRegisterMap(variable.uniqueKey))
        }

      case binding: StructObjectBinding =>
        val target = registerProvider.fresh()
        val objectName = AsmRuntimeNames.struct.`object`(binding.definition.schema)
        AsmChunk(target, PoemInstruction.GlobalGet(target, objectName))
    }
  }

}
