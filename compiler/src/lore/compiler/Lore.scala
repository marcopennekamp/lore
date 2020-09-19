package lore.compiler

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

import lore.compiler.core._
import lore.compiler.semantics.Registry
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.FilterTypeVectorExtension

import scala.util.Using

object Lore {
  lazy val pyramid: Vector[FragmentResolution] = Vector(
    fragment("pyramid.collections", Path.of("pyramid", "collections.lore")),
    fragment("pyramid.core", Path.of("pyramid", "core.lore")),
    fragment("pyramid.io", Path.of("pyramid", "io.lore")),
    fragment("pyramid.math", Path.of("pyramid", "math.lore")),
    fragment("pyramid.string", Path.of("pyramid", "string.lore")),
  )

  trait FragmentResolution
  object FragmentResolution {
    case class Success(fragment: Fragment) extends FragmentResolution
    case class NotFound(path: Path) extends FragmentResolution
  }

  def fragment(name: String, path: Path): FragmentResolution = {
    import scala.jdk.CollectionConverters._

    if (Files.notExists(path)) {
      FragmentResolution.NotFound(path)
    } else {
      val source = Files.lines(path).iterator().asScala.mkString("\n") + "\n" // Ensure that the file ends in a newline.
      FragmentResolution.Success(Fragment(name, source))
    }
  }

  /**
    * Compiles a Lore program from a single source.
    */
  def fromSingleSource(resolution: FragmentResolution): Compilation[(Registry, String)] = {
    val options = CompilerOptions(
      runtimeLogging = false,
    )
    val resolutions = pyramid :+ resolution
    val failures = resolutions.filterType[FragmentResolution.NotFound]
    if (failures.nonEmpty) {
      for (failure <- failures) {
        println(s"${FeedbackPrinter.tagError} The file '${failure.path}' does not exist!")
      }
      throw CompilationException("Not all files to be compiled were found. Consult the console output above for a list of these files.")
    } else {
      val compiler = new LoreCompiler(resolutions.filterType[FragmentResolution.Success].map(_.fragment), options)
      compiler.compile()
    }
  }

  /**
    * Compiles a Lore program from a named example within the Lore examples directory.
    */
  def fromExample(name: String): Compilation[(Registry, String)] = {
    val path = Path.of("examples", s"$name.lore")
    fromSingleSource(fragment(name, path))
  }

  /**
    * Stringifies the compilation result in a user-palatable way, discussing errors or the successful result in
    * text form.
    */
  def stringifyCompilationInfo(result: Compilation[Registry]): String = {
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
          registry.getTypes.values.map(Type.toString(_, verbose = true)).foreach(s => printer.println(s"  $s"))

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
    Files.writeString(Path.of("lore-program.js"), output)
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
