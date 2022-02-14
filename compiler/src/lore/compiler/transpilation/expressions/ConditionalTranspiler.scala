package lore.compiler.transpilation.expressions

import lore.compiler.semantics.expressions.Expression.Cond
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl._
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.transpilation.{Chunk, RuntimeApi, TemporaryVariableProvider}
import lore.compiler.types.TupleType

object ConditionalTranspiler {

  def transpile(expression: Cond, cases: Vector[(Chunk, Chunk)])(implicit variableProvider: TemporaryVariableProvider, runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): Chunk = {
    if (cases.isEmpty) {
      return Chunk.unit()
    }

    // If the result type of the `cond` is Unit, we can ignore the results of the respective bodies.
    val varResult = if (expression.tpe != TupleType.UnitType) Some(variableProvider.createVariable()) else None
    val resultDeclaration = varResult.map(_.declareMutableAs(Target.Undefined))

    def wrapBody(chunk: Chunk): Target.Block = {
      // If there is a result variable, we want to assign the chunk's expression to the variable. Otherwise, we still
      // want the expression to execute to preserve potential side effects.
      val lastStatement = varResult.map(_.assign(chunk.expression)).orElse(chunk.meaningfulExpression)
      Target.Block(chunk.statements ++ lastStatement.toVector)
    }

    def caseToChunk(condition: Chunk, body: Chunk, elsePart: TargetStatement) = {
      val ifElse = Target.IfElse(condition.expression, wrapBody(body), elsePart)
      Chunk.unit(condition.statements :+ ifElse: _*)
    }

    // The last case must be a total case.
    val (_, lastBody) = cases.last
    val lastChunk = Chunk.unit(wrapBody(lastBody))

    val combinedChunk = cases.init.foldRight(lastChunk) {
      case ((condition, body), chunk) =>
        val elsePart = if (chunk.asCode.length == 1) chunk.asCode.head else Target.Block(chunk.asCode)
        caseToChunk(condition, body, elsePart)
    }

    Chunk(
      resultDeclaration.toVector ++ combinedChunk.statements,
      varResult.getOrElse(RuntimeApi.tuples.unitValue)
    )
  }

}
