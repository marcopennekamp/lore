package lore.compiler.generation

import lore.compiler.target.Target.TargetStatement

object GenerationPhase {
  def process(statements: Vector[TargetStatement]): String = {
    statements.map(JavascriptGenerator.generate).mkString("\n")
  }
}
