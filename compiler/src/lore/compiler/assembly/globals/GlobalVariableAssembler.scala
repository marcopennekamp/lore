package lore.compiler.assembly.globals

import lore.compiler.assembly.AsmRuntimeNames
import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.poem.{PoemEagerGlobalVariable, PoemFunction, PoemGlobalVariable, PoemLazyGlobalVariable}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableAssembler {

  /**
    * Generates a PoemGlobalVariable from the given global variable and an initializer function if the variable's value
    * is lazy, including any auxiliary functions.
    */
  def generate(variable: GlobalVariableDefinition)(implicit registry: Registry): (PoemGlobalVariable, Vector[PoemFunction]) = {
    // A global variable can only be assembled as eager if its value expression can be represented as a PoemValue.
    // Otherwise, we need an initializing function and a lazy global variable.
    ValueAssembler.generate(variable.value) match {
      case Some(poemValue) => (PoemEagerGlobalVariable(variable.name, poemValue), Vector.empty)

      case None =>
        val initializerName = AsmRuntimeNames.globalVariable.initializer(variable)
        val signature = FunctionSignature(
          initializerName,
          Vector.empty,
          Vector.empty,
          variable.tpe,
          variable.position,
        )
        val generatedPoemFunctions = FunctionAssembler.generate(signature, Some(variable.value), Map.empty)

        (PoemLazyGlobalVariable(variable.name, initializerName), generatedPoemFunctions)
    }
  }

}
