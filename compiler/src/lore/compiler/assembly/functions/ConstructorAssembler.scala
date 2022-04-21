package lore.compiler.assembly.functions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.assembly.{Chunk, AsmRuntimeNames, RegisterProvider}
import lore.compiler.poem.{Poem, PoemFunctionInstance, PoemInstruction}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.types.StructType

object ConstructorAssembler {

  def generateCall(
    structType: StructType,
    valueArgumentRegs: Vector[Poem.Register],
  )(implicit registerProvider: RegisterProvider): Chunk = {
    generateCall(structType, registerProvider.fresh(), valueArgumentRegs)
  }

  def generateCall(
    structType: StructType,
    regResult: Poem.Register,
    valueArgumentRegs: Vector[Poem.Register],
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val constructorName = AsmRuntimeNames.struct.constructor(structType.schema)

    // If the call has polymorphic type arguments, i.e. a type argument contains type variables, we have to use the
    // `CallPoly` instruction. Otherwise, we can use the simpler `Call` instruction with a constant function instance.
    if (!structType.hasPolymorphicTypeArguments) {
      val poemTypeArguments = structType.typeArguments.map(TypeAssembler.generate)
      val poemFunctionInstance = PoemFunctionInstance(constructorName, poemTypeArguments)
      Chunk(regResult, PoemInstruction.Call(regResult, poemFunctionInstance, valueArgumentRegs))
    } else {
      val typeArgumentChunks = structType.typeArguments.map(TypeAssembler.generateTypeConst)
      val callInstruction = PoemInstruction.CallPoly(
        regResult,
        constructorName,
        typeArgumentChunks.map(_.forceResult),
        valueArgumentRegs,
      )
      Chunk.concat(typeArgumentChunks) ++ Chunk(regResult, callInstruction)
    }
  }

  def generateValue(expression: Expression.ConstructorValue)(implicit registerProvider: RegisterProvider): Chunk = {
    val regResult = registerProvider.fresh()
    ValueAssembler.generateConst(expression, regResult).getOrElse {
      val constructorName = AsmRuntimeNames.struct.constructor(expression.structType.schema)
      val typeArgumentChunks = expression.structType.typeArguments.map(TypeAssembler.generateTypeConst)
      val instruction = PoemInstruction.FunctionSingle(regResult, constructorName, typeArgumentChunks.map(_.forceResult))
      Chunk.concat(typeArgumentChunks) ++ Chunk(regResult, instruction)
    }
  }

  def generatePropertyDefault(property: StructPropertyDefinition)(implicit registerProvider: RegisterProvider): Chunk = {
    val functionName = AsmRuntimeNames.struct.defaultPropertyValue(property)
    val regResult = registerProvider.fresh()
    val functionInstance = PoemFunctionInstance(functionName, Vector.empty)
    Chunk(regResult, PoemInstruction.Call(regResult, functionInstance, Vector.empty))
  }

}
