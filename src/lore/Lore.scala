package lore

import java.nio.file.{Files, Path}

import lore.compiler.{C, Errors, FeedbackPrinter, LoreCompiler, Registry, Result}

object Lore {
  /* def main(args: Array[String]): Unit = {
    val maybeContext = Context.fromExample(args(0))
    if (maybeContext.isEmpty) {
      println("The context couldn't be created due to an error (likely mentioned above). Aborting Lore execution...")
      return
    }

    implicit val context = maybeContext.get

    // Print types for debugging.
    println("Types:")
    context.types.values.foreach { t =>
      println(s"  ${t.verbose}")
    }

    // Print functions for debugging.
    context.multiFunctions.values.foreach { mf =>
      println()
      println(s"${mf.name}:")
      mf.functions.foreach(f => println(s"  $f"))
    }

    println()
    val verificationResult = context.verify()
    verificationResult.print()
  } */

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
    }
  }
}
