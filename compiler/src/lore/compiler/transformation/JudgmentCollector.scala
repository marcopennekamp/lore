package lore.compiler.transformation

import lore.compiler.inference.TypingJudgment

/**
  * Collects all typing judgments created during transformation of
  */
class JudgmentCollector {

  private var _judgments: Vector[TypingJudgment] = Vector.empty

  def judgments: Vector[TypingJudgment] = this.synchronized(_judgments)

  def add(judgments: Vector[TypingJudgment]): Unit = this.synchronized {
    _judgments = _judgments ++ judgments
  }

  def add(judgments: TypingJudgment*): Unit = this.synchronized {
    add(judgments.toVector)
  }

}
