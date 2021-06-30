package lore.compiler.phases.generation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.target.Target.TargetStatement

object GenerationPhase {
  def process(statements: Vector[TargetStatement]): Compilation[String] = {
    val preamble = "import Lore from './runtime/src/lore/runtime/Lore.ts';\n"
    (Vector(preamble) ++ statements.map(JavascriptGenerator.generate)).mkString("\n").compiled
  }
}
