package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.phases.transpilation.TranspiledChunk.{JsCode, JsExpr}

/**
  * @param statements The statements that need to be executed before the given expression is evaluated.
  * @param expression The expression that represents the result value of this transpiled chunk.
  */
case class TranspiledChunk(statements: JsCode, expression: Option[JsExpr]) {
  /**
    * Maps the chunk's expression to some other expression, keeping statements intact.
    */
  def mapExpression(f: JsExpr => JsExpr): TranspiledChunk = TranspiledChunk(statements, expression.map(f))

  /**
    * Returns the chunk's code in one value, including the expression.
    */
  val code: JsCode = statements + expression.getOrElse("")
}

object TranspiledChunk {
  type JsCode = String
  type JsExpr = String

  /**
    * Creates a transpiled chunk only with an expression and an empty statement list.
    */
  def expression(expr: JsExpr): TranspiledChunk = TranspiledChunk("", Some(expr))

  /**
    * Creates a transpiled chunk that consists only of a statement list.
    */
  def statements(statements: JsCode*): TranspiledChunk = TranspiledChunk(statements.mkString("\n"), None)

  /**
    * Combines the transpiled chunks. Statements are simply concatenated, while expressions are combined according
    * to the transformation function.
    */
  def combined(chunks: List[TranspiledChunk])(transform: List[JsExpr] => JsExpr): TranspiledChunk = {
    val statements = chunks.map(_.statements).mkString("\n")
    val expressions = chunks.map(_.expression.getOrElse(throw new RuntimeException("Compiler bug! This must be an expression.")))
    TranspiledChunk(statements, Some(transform(expressions)))
  }

  /**
    * A special variant of [[combined]] that uses operators.
    */
  def operatorChain(chunks: List[TranspiledChunk], operator: String): TranspiledChunk = combined(chunks)(_.mkString(operator))

  /**
    * A binary variant of [[operatorChain]].
    */
  def binary(left: TranspiledChunk, right: TranspiledChunk, operator: String): TranspiledChunk = operatorChain(List(left, right), operator)

  /**
    * Sequences the list of transpiled chunks, concatenating each chunk's statement list AND expression,
    * except for the last chunk, whose expression becomes the expression of the resulting chunk.
    */
  def sequencedIdentity(chunks: List[TranspiledChunk]): TranspiledChunk = sequenced(chunks)(TranspiledChunk.apply)

  /**
    * Sequences the list of transpiled chunks, concatenating each chunk's statement list AND expression,
    * except for the last chunks, whose expression is the second value of the transformation function.
    */
  def sequenced(chunks: List[TranspiledChunk])(transform: (JsCode, Option[JsExpr]) => TranspiledChunk): TranspiledChunk = {
    val statements = (chunks.dropRight(1).map(_.code) :+ chunks.lastOption.map(_.statements).getOrElse("")).mkString("\n")
    val expression = chunks.lastOption.flatMap(_.expression)
    transform(statements, expression)
  }

  implicit class ListExtension(chunks: List[TranspiledChunk]) {
    def combined(transform: List[JsExpr] => JsExpr): TranspiledChunk = TranspiledChunk.combined(chunks)(transform)
    def operatorChain(operator: String): TranspiledChunk = TranspiledChunk.operatorChain(chunks, operator)
    def sequencedIdentity: TranspiledChunk = TranspiledChunk.sequencedIdentity(chunks)
    def sequenced(transform: (JsCode, Option[JsExpr]) => TranspiledChunk): TranspiledChunk = TranspiledChunk.sequenced(chunks)(transform)
  }
}

object Transpilation {
  type Transpilation = Compilation[TranspiledChunk]

  def chunk(statements: JsCode, expression: Option[JsExpr]): Transpilation = {
    Compilation.succeed(TranspiledChunk(statements, expression))
  }
  def statements(statements: JsCode*): Transpilation = {
    Compilation.succeed(TranspiledChunk.statements(statements: _*))
  }
  def expression(expression: JsExpr): Transpilation = {
    Compilation.succeed(TranspiledChunk.expression(expression))
  }
  def combined(chunks: List[TranspiledChunk])(transform: List[JsExpr] => JsExpr): Transpilation = {
    Compilation.succeed(chunks.combined(transform))
  }
  def operatorChain(chunks: List[TranspiledChunk], operator: String): Transpilation = {
    Compilation.succeed(chunks.operatorChain(operator))
  }
  def binary(left: TranspiledChunk, right: TranspiledChunk, operator: String): Transpilation = {
    Compilation.succeed(TranspiledChunk.binary(left, right, operator))
  }
  def sequencedIdentity(chunks: List[TranspiledChunk]): Transpilation = {
    Compilation.succeed(chunks.sequencedIdentity)
  }
  def sequenced(chunks: List[TranspiledChunk])(transform: (JsCode, Option[JsExpr]) => TranspiledChunk): Transpilation = {
    Compilation.succeed(chunks.sequenced(transform))
  }
}
