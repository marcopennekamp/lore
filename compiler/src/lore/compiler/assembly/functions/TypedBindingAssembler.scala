package lore.compiler.assembly.functions

import lore.compiler.assembly.{Chunk, RuntimeNames, RegisterProvider}
import lore.compiler.poem.PoemInstruction
import lore.compiler.semantics.scopes.{LocalVariable, StructObjectBinding, TypedBinding}
import lore.compiler.semantics.variables.GlobalVariableDefinition

object TypedBindingAssembler {

  def generate(binding: TypedBinding)(
    implicit registerProvider: RegisterProvider,
    variableRegisterMap: VariableRegisterMap,
    capturedVariableMap: CapturedVariableMap,
  ): Chunk = {
    binding match {
      case global: GlobalVariableDefinition =>
        val target = registerProvider.fresh()
        Chunk(target, PoemInstruction.GlobalGet(target, global.name))

      case variable: LocalVariable =>
        // If the variable is a captured variable, we need to load its value from the lambda context via `LambdaLocal`.
        capturedVariableMap.get(variable.uniqueKey) match {
          case Some(index) =>
            val target = registerProvider.fresh()
            Chunk(target, PoemInstruction.LambdaLocal(target, index))

          case None =>
            Chunk(variableRegisterMap(variable.uniqueKey))
        }

      case binding: StructObjectBinding =>
        val target = registerProvider.fresh()
        val objectName = RuntimeNames.struct.`object`(binding.definition.schema)
        Chunk(target, PoemInstruction.GlobalGet(target, objectName))
    }
  }

}
