package lore.compiler.assembly.functions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.{AsmChunk, AsmRuntimeNames, RegisterProvider}
import lore.compiler.poem.{Poem, PoemFunctionInstance, PoemInstruction}
import lore.compiler.types.{StructType, Type, TypeVariable}

object ConstructorCallAssembler {

  def generate(
    structType: StructType,
    regResult: Poem.Register,
    valueArgumentRegs: Vector[Poem.Register],
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    // TODO (assembly): We can inline the `Struct` instruction if the struct doesn't need a constructor function, which
    //                  is the case when it has no open type parameters.
    val constructorName = AsmRuntimeNames.struct.construct(structType.schema)

    // If the call has polymorphic type arguments, i.e. a type argument contains type variables, we have to use the
    // `CallPoly` instruction. Otherwise, we can use the simpler `Call` instruction with a constant function instance.
    val hasPolymorphicTypeArguments = structType.typeArguments.exists(Type.isPolymorphic)
    if (!hasPolymorphicTypeArguments) {
      val poemTypeArguments = structType.typeArguments.map(TypeAssembler.generate)
      val poemFunctionInstance = PoemFunctionInstance(constructorName, poemTypeArguments)
      AsmChunk(regResult, PoemInstruction.Call(regResult, poemFunctionInstance, valueArgumentRegs))
    } else {
      val typeArgumentChunks = structType.typeArguments.map { typeArgument =>
        val regTypeArgument = registerProvider.fresh()

        // `TypeConst` will substitute type variables in `typeArgument` if it contains any. Using `TypeArg` is a simple
        // optimization that isn't necessary for correctness.
        val instruction = typeArgument match {
          case tv: TypeVariable => PoemInstruction.TypeArg(regTypeArgument, tv.index)
          case _ => PoemInstruction.TypeConst(regTypeArgument, TypeAssembler.generate(typeArgument))
        }

        AsmChunk(regTypeArgument, instruction)
      }

      val callInstruction = PoemInstruction.CallPoly(
        regResult,
        constructorName,
        typeArgumentChunks.map(_.forceResult),
        valueArgumentRegs,
      )
      AsmChunk.concat(typeArgumentChunks) ++ AsmChunk(regResult, callInstruction)
    }
  }

}
