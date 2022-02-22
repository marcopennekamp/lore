package lore.compiler.assembly.functions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
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
        val instruction = PoemInstruction.GlobalGet(target, global)
        AsmChunk(target, instruction)

      case variable: LocalVariable =>
        // If the variable is a captured variable, we need to load its value from the lambda context via `LambdaLocal`.
        capturedVariableMap.get(variable.uniqueKey) match {
          case Some(index) =>
            val target = registerProvider.fresh()
            AsmChunk(target, PoemInstruction.LambdaLocal(target, index))

          case None =>
            AsmChunk(variableRegisterMap(variable.uniqueKey))
        }

      // TODO (assembly): Implement. Struct objects aren't directly supported by the VM. Instead, we have to define a
      //                  lazy global variable. The target representation would then be a `GlobalGet` instruction.
      //                  However, we could even go as far as rolling StructObjectBindings into GlobalVariableDefinitions
      //                  in the compiler. This might simplify the whole StructBinding business.
      case _: StructObjectBinding => ???
    }
  }

}
