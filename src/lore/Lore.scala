package lore

import java.nio.file.{Files, Path}

import lore.compiler.Compilation.C
import lore.compiler.feedback.FeedbackPrinter
import lore.compiler.{Errors, LoreCompiler, Registry, Result}
import lore.types.DeclaredType

object Lore {
  def fromExample(name: String): C[Registry] = {
    import scala.jdk.CollectionConverters._

    val sourcePath = Path.of("examples", s"$name.lore")
    val source = Files.lines(sourcePath).iterator().asScala.toList.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    val compiler = new LoreCompiler(List(
      LoreCompiler.SourceFragment(name, source)
    ))
    compiler.compile()
  }

  def main(args: Array[String]): Unit = {
    val result = fromExample(args(0))
    result match {
      case Errors(errors, infos) =>
        val feedback = errors ++ infos
        println()
        println(s"${FeedbackPrinter.tagError} Compilation failed with errors:")
        if (feedback.nonEmpty) println(FeedbackPrinter.print(feedback))
        println()
      case Result(registry, infos) =>
        println()
        println(s"${FeedbackPrinter.tagSuccess} Compilation was successful:")
        if(infos.nonEmpty) println(FeedbackPrinter.print(infos))
        println(s"${FeedbackPrinter.tagSuccess} Compilation result: $registry")
        println()

        // Print types for debugging.
        println("Types:")
        registry.getTypes.values.map {
          case t: DeclaredType => t.verbose
          case t => t.toString
        }.foreach(s => println(s"  $s"))

        // Print functions for debugging.
        registry.getMultiFunctions.values.foreach { mf =>
          println()
          println(s"${mf.name}:")
          mf.functions.foreach(f => println(s"  $f"))
        }
    }
  }
}
