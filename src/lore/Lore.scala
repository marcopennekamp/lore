package lore

import java.nio.file.{Files, Path}

import lore.compiler.{Errors, FeedbackPrinter, LoreCompiler, Result}

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

  def main(args: Array[String]): Unit = {
    import scala.jdk.CollectionConverters._

    val exampleName = args(0)
    val sourcePath = Path.of("examples", s"$exampleName.lore")
    val source = Files.lines(sourcePath).iterator().asScala.toList.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    val compiler = new LoreCompiler(List(
      LoreCompiler.SourceFragment(exampleName, source)
    ))
    val result = compiler.compile()
    result match {
      case Errors(errors, infos) =>
        val feedback = errors ++ infos
        println()
        println(s"${FeedbackPrinter.tagError} Compilation failed with errors:")
        if (feedback.nonEmpty) println(FeedbackPrinter.print(feedback))
        println()
      case Result(value, infos) =>
        println()
        println(s"${FeedbackPrinter.tagSuccess} Compilation was successful:")
        if(infos.nonEmpty) println(FeedbackPrinter.print(infos))
        println(s"${FeedbackPrinter.tagSuccess} Compilation result: $value")
        println()
        // TODO: Do something with the result.
    }
  }
}
