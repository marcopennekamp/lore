package lore.compiler.transpilation.expressions

import lore.compiler.core.Position
import lore.compiler.semantics.expressions.Expression.{Cond, CondCase, IfElse, Literal}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl._
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.transpilation.{Chunk, RuntimeApi, TemporaryVariableProvider}
import lore.compiler.types.{BasicType, TupleType}

case class ConditionalTranspiler()(implicit variableProvider: TemporaryVariableProvider, runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory) {

  /**
    * Transpiles an `if` expression by delegating to the `cond` transpiler.
    */
  def transpile(expression: IfElse)(condition: Chunk, onTrue: Chunk, onFalse: Chunk): Chunk = {
    val cases = Vector(
      CondCase(expression.condition, expression.onTrue),
      CondCase(Literal(true, BasicType.Boolean, Position.internal), expression.onFalse),
    )
    val caseChunks = Vector(
      (condition, onTrue),
      (Chunk.expression(Target.BooleanLiteral(true)), onFalse),
    )
    val cond = Cond(cases, expression.tpe, expression.position)
    transpile(cond)(caseChunks)
  }

  def transpile(expression: Cond)(cases: Vector[(Chunk, Chunk)]): Chunk = {
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

    val (lastCondition, lastBody) = cases.last
    val lastChunk = if (expression.cases.last.isTotalCase) {
      // If the last case is total, we can simply transpile it as a block without an `if` part.
      Chunk.unit(wrapBody(lastBody))
    } else {
      // If the `cond` isn't total, we have to assign the unit value to the result variable.
      val elsePart = if (!expression.isTotal) varResult.map(_.assign(RuntimeApi.tuples.unitValue)).getOrElse(Target.Empty) else Target.Empty
      caseToChunk(lastCondition, lastBody, elsePart)
    }

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
