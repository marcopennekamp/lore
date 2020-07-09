package lore.compiler

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

import lore.compiler.core.Compilation.C
import lore.compiler.core.{Errors, Registry, Result}
import lore.compiler.feedback.FeedbackPrinter
import lore.compiler.types.DeclaredType

import scala.util.Using

object Lore {
  val pyramid = List(
    fragment("pyramid.collections", Path.of("pyramid", "collections.lore")),
    fragment("pyramid.io", Path.of("pyramid", "io.lore")),
    fragment("pyramid.string", Path.of("pyramid", "string.lore")),
  )

  def fragment(name: String, path: Path): LoreCompiler.SourceFragment = {
    import scala.jdk.CollectionConverters._
    val source = Files.lines(path).iterator().asScala.mkString("\n") + "\n" // Ensure that the file ends in a newline.
    LoreCompiler.SourceFragment(name, source)
  }

  /**
    * Compiles a Lore program from a single source.
    */
  def fromSingleSource(fragment: LoreCompiler.SourceFragment): C[(Registry, String)] = {
    val options = CompilerOptions(
      runtimeLogging = false,
    )
    val compiler = new LoreCompiler(pyramid ++ List(fragment), options)
    compiler.compile()
  }

  /**
    * Compiles a Lore program from a named example within the Lore examples directory.
    */
  def fromExample(name: String): C[(Registry, String)] = {
    val path = Path.of("examples", s"$name.lore")
    fromSingleSource(fragment(name, path))
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
    Runtime.getRuntime.exec("prettier --write lore-program.js")
  }

  def main(args: Array[String]): Unit = {
    val beforeCompile = System.nanoTime()
    val result = fromExample(args(0))
    val afterCompile = System.nanoTime()
    println(stringifyCompilationInfo(result.map(_._1)))
    result.map(_._2).foreach(writeResult)
    println(s"Compile time: ${((afterCompile - beforeCompile) / 1000) / 1000.0}ms.")
  }
}
