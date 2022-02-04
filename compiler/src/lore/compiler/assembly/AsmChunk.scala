package lore.compiler.assembly

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction}

// TODO (assembly): Rename this to Chunk once we've deleted the `transpilation` stuff.

/**
  * A Chunk bundles a sequence of instructions together with an optional result register. This result can be used by
  * subsequent chunks.
  */
case class AsmChunk(result: Option[Poem.Register], instructions: Vector[PoemInstruction]) {
  /**
    * Gets `result` or throws a CompilationException. This should be used when a result has to exist.
    */
  def forceResult(position: Position): Poem.Register = result match {
    case Some(value) => value
    case None => throw CompilationException(s"This Chunk must have a result. Position: $position.")
  }

  /**
    * Concatenates this chunk with `other`, concatenating instructions and preserving the `result` of `other`.
    */
  def ++(other: AsmChunk): AsmChunk = AsmChunk(other.result, this.instructions ++ other.instructions)
}

object AsmChunk {
  val empty: AsmChunk = AsmChunk()

  def apply(result: Poem.Register, instructions: PoemInstruction*): AsmChunk = AsmChunk(Some(result), instructions.toVector)

  /**
    * Creates a new Chunk without a result register.
    */
  def apply(instructions: PoemInstruction*): AsmChunk = AsmChunk(None, instructions.toVector)

  def concat(chunks: Vector[AsmChunk]): AsmChunk = chunks.foldLeft(AsmChunk.empty)(_ ++ _)
}
