package lore.compiler.assembly.globals

import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.assembly.{Chunk, AsmRuntimeNames}
import lore.compiler.core.Position
import lore.compiler.poem.{PoemEagerGlobalVariable, PoemFunction, PoemGlobalVariable, PoemLazyGlobalVariable}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.types.Type

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
      case None => generateLazyGlobalVariable(variable.name, variable.tpe, Left(variable.value), variable.position)
    }
  }

  /**
    * Generates a lazy global variable and its initializer function from the given variable `name`, `tpe`, and `value`.
    * This function doesn't take a GlobalVariableDefinition because it is also used to generate poem global variables
    * that aren't available as GlobalVariableDefinitions.
    */
  def generateLazyGlobalVariable(
    name: NamePath,
    tpe: Type,
    value: Either[Expression, Chunk],
    position: Position,
  )(implicit registry: Registry): (PoemLazyGlobalVariable, Vector[PoemFunction]) = {
    val initializerName = AsmRuntimeNames.globalVariable.initializer(name)
    val signature = FunctionSignature(
      initializerName,
      Vector.empty,
      Vector.empty,
      tpe,
      position,
    )
    val generatedPoemFunctions = FunctionAssembler.generate(signature, value)
    (PoemLazyGlobalVariable(name, initializerName), generatedPoemFunctions)
  }

}
