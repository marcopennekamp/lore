package lore.compiler.assembly.expressions

import lore.compiler.assembly.optimization.{ConstSmasher, RegisterAllocator}
import lore.compiler.poem.PoemInstruction
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.ParameterDefinition

object ExpressionAssembler {

  /**
    * Generates instructions for the given expression. This always attaches a `Return` instruction to the end.
    */
  def generate(
    parameters: Vector[ParameterDefinition],
    expression: Expression,
  )(implicit registry: Registry): Vector[PoemInstruction] = {
    val initialChunk = ExpressionVisitor.visit(new ExpressionAssemblyVisitor(parameters, expression.position))(expression)
    // TODO (assembly): Maybe use `Return0` and `ReturnUnit` if possible.
    var instructions = initialChunk.instructions :+ PoemInstruction.Return(initialChunk.forceResult(expression.position))
    instructions = LabelResolver.resolve(instructions, expression.position)
    instructions = ConstSmasher.optimize(instructions)
    instructions = RegisterAllocator.optimize(instructions)
    instructions
  }

}
