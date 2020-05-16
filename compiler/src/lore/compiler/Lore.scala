package lore.compiler

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

import lore.compiler.Compilation.C
import lore.compiler.feedback.FeedbackPrinter
import lore.compiler.types.DeclaredType

import scala.util.Using

object Lore {
  /**
    * Compiles a Lore program from a single source.
    */
  def fromSingleSource(name: String, source: String): C[(Registry, String)] = {
    val compiler = new LoreCompiler(List(
      LoreCompiler.SourceFragment(
        name,
        source + "\n" // Ensure that the file ends in a newline.
      )
    ))
    compiler.compile()
  }

  /**
    * Compiles a Lore program from a named example within the Lore examples directory.
    */
  def fromExample(name: String): C[(Registry, String)] = {
    import scala.jdk.CollectionConverters._
    val sourcePath = Path.of("examples", s"$name.lore")
    val source = Files.lines(sourcePath).iterator().asScala.mkString("\n") + "\n"
    fromSingleSource(name, source)
  }

  /**
    * Stringifies the compilation result in a user-palatable way, discussing errors or the successful result in
    * text form.
    */
  def stringifyCompilationInfo(result: C[Registry]): String = {
    val out = new ByteArrayOutputStream()
    Using(new PrintStream(out, true, "utf-8")) { printer =>
      // Print either errors or the compilation result to the output stream.
      result match {
        case Errors(errors, infos) =>
          val feedback = errors ++ infos
          printer.println()
          printer.println(s"${FeedbackPrinter.tagError} Compilation failed with errors:")
          if (feedback.nonEmpty) printer.println(FeedbackPrinter.print(feedback))
        case Result(registry, infos) =>
          printer.println()
          printer.println(s"${FeedbackPrinter.tagSuccess} Compilation was successful:")
          if(infos.nonEmpty) printer.println(FeedbackPrinter.print(infos))
          printer.println(s"${FeedbackPrinter.tagSuccess} Compilation result: $registry")

          // Print types for debugging.
          printer.println()
          printer.println("Types:")
          registry.getTypes.values.map {
            case t: DeclaredType => t.verbose
            case t => t.toString
          }.foreach(s => printer.println(s"  $s"))

          // Print functions for debugging.
          registry.getMultiFunctions.values.foreach { mf =>
            printer.println()
            printer.println(s"${mf.name}:")
            mf.functions.foreach(f => printer.println(s"  $f"))
          }
      }

      // Finally, return the constructed string.
      out.toString("utf-8")
    }.get // There should be no exceptions here, as we are not trying to access any files.
  }

  /**
    * Writes the result of the compilation to the file system.
    */
  def writeResult(output: String): Unit = {
    Files.writeString(Path.of(s"lore-program.js"), output)
  }

  def main(args: Array[String]): Unit = {
    val result = fromExample(args(0))
    println(stringifyCompilationInfo(result.map(_._1)))
    result.map(_._2).foreach(writeResult)
  }
}
