package lore.compiler.assembly

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction}

/**
  * A Chunk bundles a sequence of instructions together with an optional result register. This result can be used by
  * subsequent chunks.
  *
  * `postLabels` are labels which point to the next instruction <i>after</i> this chunk. When this chunk is
  * concatenated with other chunks, its post labels are resolved.
  */
case class Chunk(
  result: Option[Poem.Register],
  instructions: Vector[PoemInstruction],
  postLabels: Set[Poem.Label],
) {
  /**
    * Gets `result` or throws a CompilationException. This should be used when a result has to exist.
    */
  def forceResult(position: Position): Poem.Register = result match {
    case Some(value) => value
    case None => throw CompilationException(s"This Chunk must have a result. Position: $position.")
  }

  def forceResult: Poem.Register = result match {
    case Some(value) => value
    case None => throw CompilationException(s"This Chunk must have a result.")
  }

  /**
    * Concatenates this chunk with `other`, concatenating instructions and preserving the `result` of `other`.
    *
    * All post labels of this chunk are added to the first instruction of `other`, or become post labels of the
    * resulting chunk if `other` has no instructions. Post labels of `other` are preserved.
    */
  def ++(other: Chunk): Chunk = {
    if (other.instructions.isEmpty) {
      Chunk(other.result, instructions, postLabels ++ other.postLabels)
    } else {
      other.instructions.head.addLabels(postLabels)
      Chunk(other.result, instructions ++ other.instructions, other.postLabels)
    }
  }

  /**
    * Attaches the given label as a post label to this chunk, returning a new chunk.
    */
  def withPostLabel(label: Poem.Label): Chunk = {
    Chunk(result, instructions, postLabels + label)
  }

  /**
    * Adds a label to the <i>first</i> instruction of the chunk.
    */
  def labelFirst(label: Poem.Label): Unit = {
    instructions.headOption match {
      case Some(head) => head.addLabel(label)
      case None => throw CompilationException("Cannot add a label to an empty chunk.")
    }
  }
}

object Chunk {
  val empty: Chunk = Chunk()

  def apply(result: Poem.Register, instructions: Vector[PoemInstruction]): Chunk = Chunk(Some(result), instructions, Set.empty[Poem.Label])
  def apply(result: Poem.Register, instructions: PoemInstruction*): Chunk = Chunk(result, instructions.toVector)

  /**
    * Creates a new Chunk without a result register.
    */
  def apply(instructions: Vector[PoemInstruction]): Chunk = Chunk(None, instructions, Set.empty[Poem.Label])
  def apply(instructions: PoemInstruction*): Chunk = Chunk(instructions.toVector)
  def apply(instructions: Vector[PoemInstruction], instructions2: PoemInstruction*): Chunk = Chunk(instructions ++ instructions2)

  def concat(chunks: Vector[Chunk]): Chunk = chunks.foldLeft(Chunk.empty)(_ ++ _)
}
