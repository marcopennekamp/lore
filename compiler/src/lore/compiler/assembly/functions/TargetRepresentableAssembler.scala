package lore.compiler.assembly.functions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem.PoemInstruction
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.scopes.{LocalVariable, StructObjectBinding}
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.target.TargetRepresentable

object TargetRepresentableAssembler {

  def generate(targetRepresentable: TargetRepresentable)(
    implicit registerProvider: RegisterProvider,
    variableRegisterMap: VariableRegisterMap,
  ): AsmChunk = {
    targetRepresentable match {
      case module: GlobalModule =>
        throw CompilationException(s"Modules don't have a representation in poem bytecode. Module name: ${module.name}.")

      case global: GlobalVariableDefinition =>
        val target = registerProvider.fresh()
        val instruction = PoemInstruction.GlobalGet(target, global)
        AsmChunk(target, instruction)

      // TODO (assembly): Implement.
      case _: MultiFunctionDefinition => ???

      // TODO (assembly): Implement.
      case _: FunctionDefinition => ???

      case variable: LocalVariable => AsmChunk(variableRegisterMap(variable.uniqueKey))

      // TODO (assembly): Implement. Struct objects aren't directly supported by the VM. Instead, we have to define a
      //                  lazy global variable. The target representation would then be a `GlobalGet` instruction.
      //                  However, we could even go as far as rolling StructObjectBindings into GlobalVariableDefinitions
      //                  in the compiler. This might simplify the whole StructBinding business.
      case _: StructObjectBinding => ???
    }
  }

}
