package lore.compiler.assembly.functions

import lore.compiler.assembly.Chunk
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemInstruction, PoemIntValue, PoemIntrinsic}
import lore.compiler.semantics.expressions.Expression

object IntrinsicAssembler {

  /**
    * Generates an intrinsic call. Some intrinsics known by the compiler such as `lore.list.get!` aren't actually VM
    * intrinsics, but translate to other instructions such as `ListGet`. This function handles these special cases.
    */
  def generate(
    expression: Expression.Call,
    intrinsic: PoemIntrinsic,
    regResult: Poem.Register,
    valueArgumentRegs: Vector[Poem.Register],
  ): Chunk = {
    val instruction = intrinsic.name match {
      case "lore.tuple.get!" =>
        val index = ValueAssembler.generate(expression.arguments(1)) match {
          case Some(PoemIntValue(index)) => index
          case None => throw CompilationException("`lore.tuple.get` can only be used with constant indices. This will change in the future.")
        }
        PoemInstruction.TupleGet(regResult, valueArgumentRegs(0), index.toInt)

      case "lore.list.get!" => PoemInstruction.ListGet(regResult, valueArgumentRegs(0), valueArgumentRegs(1))
      case "lore.list.length" => PoemInstruction.ListLength(regResult, valueArgumentRegs(0))

      case _ =>
        if (intrinsic.isVirtual) {
          throw CompilationException(s"Virtual intrinsic ${intrinsic.name} must be handled specially.")
        }
        PoemInstruction.Intrinsic(regResult, intrinsic, valueArgumentRegs)
    }
    Chunk(regResult, instruction)
  }

}
