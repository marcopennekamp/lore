package lore.compiler.phases.transpilation

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.transpilation.TranspiledChunk.{JsCode, JsExpr}

// TODO: This API is messy and absolutely needs another refactoring pass. Maybe we could merge the notions of
//       TranspiledChunk and Transpilation (at least in the public-facing interface).

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
    * Flat maps the transpiled chunk with the given mapping function. If expression is None, `this` transpiled chunk
    * is returned without further modification.
    */
  def flatMap(f: JsExpr => TranspiledChunk): TranspiledChunk = {
    expression.map(f).map { chunk =>
      TranspiledChunk(
        TranspiledChunk.concatStatements(statements, chunk.statements),
        chunk.expression
      )
    } getOrElse this
  }

  /**
    * Returns the chunk's code in one value, including the expression.
    */
  val code: JsCode = statements + expression.getOrElse("")
}

object TranspiledChunk {
  type JsCode = String
  type JsExpr = String

  /**
    * Concatenates the given statement lists.
    */
  def concatStatements(statements: JsCode*): JsCode = statements.mkString("\n")

  /**
    * Creates a transpiled chunk only with an expression and an empty statement list.
    */
  def expression(expr: JsExpr): TranspiledChunk = TranspiledChunk("", Some(expr))

  /**
    * Creates a transpiled chunk with the given statements and an expression.
    */
  def chunk(statements: List[JsCode], expr: JsExpr): TranspiledChunk = TranspiledChunk(concatStatements(statements: _*), Some(expr))

  /**
    * Creates a transpiled chunk that consists only of a statement list.
    */
  def statements(statements: JsCode*): TranspiledChunk = TranspiledChunk(concatStatements(statements: _*), None)

  /**
    * Combines the transpiled chunks. Statements are simply concatenated, while expressions are combined according
    * to the transformation function.
    */
  def combined(chunks: List[TranspiledChunk])(transform: List[JsExpr] => JsExpr): TranspiledChunk = {
    val statements = chunks.map(_.statements).mkString("\n")
    val expressions = chunks.map(_.expression.getOrElse(throw CompilationException("This must be an expression.")))
    TranspiledChunk(statements, Some(transform(expressions)))
  }

  /**
    * A special variant of [[combined]] that uses operators.
    *
    * @param wrap Whether to wrap the resulting expression in parentheses.
    */
  def operatorChain(chunks: List[TranspiledChunk], operator: String, wrap: Boolean = false): TranspiledChunk = {
    combined(chunks) { expressions =>
      val result = expressions.mkString(operator)
      if (wrap) s"($result)" else result
    }
  }

  /**
    * A binary variant of [[operatorChain]].
    *
    * @param wrap Whether to wrap the resulting expression in parentheses.
    */
  def binary(left: TranspiledChunk, right: TranspiledChunk, operator: String, wrap: Boolean = false): TranspiledChunk = {
    operatorChain(List(left, right), operator, wrap)
  }

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
    def operatorChain(operator: String, wrap: Boolean = false): TranspiledChunk = TranspiledChunk.operatorChain(chunks, operator, wrap)
    def sequencedIdentity: TranspiledChunk = TranspiledChunk.sequencedIdentity(chunks)
    def sequenced(transform: (JsCode, Option[JsExpr]) => TranspiledChunk): TranspiledChunk = TranspiledChunk.sequenced(chunks)(transform)
  }
}

object Transpilation {
  type Transpilation = Compilation[TranspiledChunk]

  def chunk(statements: JsCode, expression: JsExpr): Transpilation = {
    Compilation.succeed(TranspiledChunk(statements, Some(expression)))
  }
  def chunk(statements: List[JsCode], expression: JsExpr): Transpilation = {
    Compilation.succeed(TranspiledChunk.chunk(statements, expression))
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
  def operatorChain(chunks: List[TranspiledChunk], operator: String, wrap: Boolean = false): Transpilation = {
    Compilation.succeed(chunks.operatorChain(operator, wrap))
  }
  def binary(left: TranspiledChunk, right: TranspiledChunk, operator: String, wrap: Boolean = false): Transpilation = {
    Compilation.succeed(TranspiledChunk.binary(left, right, operator, wrap))
  }
  def sequencedIdentity(chunks: List[TranspiledChunk]): Transpilation = {
    Compilation.succeed(chunks.sequencedIdentity)
  }
  def sequenced(chunks: List[TranspiledChunk])(transform: (JsCode, Option[JsExpr]) => TranspiledChunk): Transpilation = {
    Compilation.succeed(chunks.sequenced(transform))
  }
}
