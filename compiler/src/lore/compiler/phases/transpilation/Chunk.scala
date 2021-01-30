package lore.compiler.phases.transpilation

import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.target.Target.{TargetExpression, TargetStatement}

/**
  * A chunk is a unit of the target representation that requires a preamble in the form of statements and also results
  * in an expression. The contract requires that the statements need to be executed before the expression.
  *
  * The expression should be seen as a transitive object. It is not always intended to become part of the actual
  * target code and must be explicitly included if desired (e.g. with a function such as `sequence`. Usually, the
  * expression is consumed by an operation using the chunk to build a bigger chunk. See for example the `flatMap` and
  * `combine` functions. That said, it is not advisable to just throw away the expression implicitly. In some cases,
  * such as with blocks, expressions must also be part of the generated code. This decision should be made deliberately.
  */
case class Chunk(statements: Vector[TargetStatement], expression: TargetExpression) {

  /**
    * The chunk's expression is <i>meaningful</i> if it is not equal to the unit value. Hence, this value contains the
    * chunk's expression unless the expression is the unit value.
    */
  lazy val meaningfulExpression: Option[TargetExpression] = if (expression != RuntimeApi.tuples.unitValue) Some(expression) else None

  /**
    * Maps the chunk's expression to some other expression, keeping statements intact.
    */
  def mapExpression(f: TargetExpression => TargetExpression): Chunk = Chunk(statements, f(expression))

  /**
    * Maps this chunk's expression with a function to create a new chunk. This chunk's statements are preserved as a
    * preamble to the new chunk. This chunk's expression is consumed in the process and not part of the new statement
    * list.
    */
  def flatMap(f: TargetExpression => Chunk): Chunk = {
    val chunk = f(expression)
    Chunk(statements ++ chunk.statements, chunk.expression)
  }

  /**
    * Translates the chunk to the target representation by wrapping the chunk's code in a block and returning the
    * expression from the block.
    */
  def asBody: Target.Block = Target.Block(statements :+ Target.Return(expression))

  /**
    * An "all executable" view on the chunk. Returns the statements concatenated with the expression, though a unit
    * value expression is ignored.
    */
  def asCode: Vector[TargetStatement] = statements ++ meaningfulExpression.toVector

}

object Chunk {

  /**
    * Creates a chunk with the given statements, using a unit value as the expression.
    */
  def unit(statements: TargetStatement*): Chunk = Chunk(statements.toVector, RuntimeApi.tuples.unitValue)

  /**
    * Creates a chunk with no statements and the given expression.
    */
  def expression(expression: TargetExpression): Chunk = Chunk(Vector.empty, expression)

  /**
    * Combines the given chunks, producing a new chunk from the expressions of the chunks. The statements of all
    * chunks and the created chunk are concatenated in order, while the expression of the created chunk becomes the
    * expression of the result chunk.
    */
  def combine(chunks: Vector[Chunk])(f: Vector[TargetExpression] => Chunk): Chunk = {
    val createdChunk = f(chunks.map(_.expression))
    Chunk(chunks.flatMap(_.statements) ++ createdChunk.statements, createdChunk.expression)
  }

  def combine(chunks: Chunk*): (Vector[TargetExpression] => Chunk) => Chunk = combine(chunks.toVector)

  /**
    * Combines the given operand chunks, combining their expressions into a [[Target.Operation]].
    *
    * @see [[Chunk.combine]]
    */
  def operation(operator: TargetOperator, operands: Chunk*): Chunk = {
    combine(operands.toVector)(expressions => Chunk.expression(Target.Operation(operator, expressions)))
  }

  /**
    * Sequences the given chunks, concatenating all but the last chunk as code (including each chunk's expression).
    * The last chunk's statements are appended to the concatenated code, while its expression becomes the new chunk's
    * expression.
    *
    * If the chunk list is empty, the function produces a chunk with no statements and the unit value as its expression.
    */
  def sequence(chunks: Vector[Chunk]): Chunk = {
    val statements = chunks.dropRight(1).flatMap(_.asCode) ++ chunks.lastOption.map(_.statements).getOrElse(Vector.empty)
    val expression = chunks.lastOption.map(_.expression).getOrElse(RuntimeApi.tuples.unitValue)
    Chunk(statements, expression)
  }

}
