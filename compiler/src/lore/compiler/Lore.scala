package lore.compiler

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

import lore.compiler.core._
import lore.compiler.semantics.Registry
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

import scala.util.Using

object Lore {
  def pyramid(baseDirectory: Path): Vector[FragmentResolution] = Vector(
    fragment("pyramid.collections", baseDirectory.resolve(Path.of("pyramid", "collections.lore"))),
    fragment("pyramid.core", baseDirectory.resolve(Path.of("pyramid", "core.lore"))),
    fragment("pyramid.introspection", baseDirectory.resolve(Path.of("pyramid", "introspection.lore"))),
    fragment("pyramid.io",baseDirectory.resolve( Path.of("pyramid", "io.lore"))),
    fragment("pyramid.math", baseDirectory.resolve(Path.of("pyramid", "math.lore"))),
    fragment("pyramid.string", baseDirectory.resolve(Path.of("pyramid", "string.lore"))),
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

  def toDirectoryPath(directory: String): Path = {
    val path = Path.of(directory)
    if (!path.toFile.exists) {
      throw new RuntimeException(s"The directory $path does not exist.")
    }
    if (!path.toFile.isDirectory) {
      throw new RuntimeException(s"The directory $path is not a directory.")
    }
    path
  }

  /**
    * Compiles a Lore program from the given base directory and relative fragment paths (including the file extension).
    */
  def fromSources(baseDirectory: Path, paths: Path*): Compilation[(Registry, String)] = {
    val options = CompilerOptions(
      runtimeLogging = false,
    )
    val resolutions = paths.map { path => fragment(path.toString, baseDirectory.resolve(path)) }
    val allResolutions = pyramid(baseDirectory) ++ resolutions
    val failures = allResolutions.filterType[FragmentResolution.NotFound]
    if (failures.nonEmpty) {
      for (failure <- failures) {
        println(s"${FeedbackPrinter.tagError} The file '${failure.path}' does not exist!")
      }
      throw new RuntimeException("Not all files to be compiled were found. Consult the console output above for a list of these files.")
    } else {
      val compiler = new LoreCompiler(allResolutions.filterType[FragmentResolution.Success].map(_.fragment), options)
      compiler.compile()
    }
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
          printer.println(s"${FeedbackPrinter.tagSuccess} Compilation was successful.")
          if(infos.nonEmpty) printer.println(FeedbackPrinter.print(infos))

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
  def writeResult(baseDirectory: Path)(output: String): Unit = {
    val path = baseDirectory.resolve("lore-program.js")
    Files.writeString(path, output)
    Runtime.getRuntime.exec(s"prettier --write ${path.toString}")
  }

  /**
    * Invokes the compiler for any number of sources given the following arguments:
    *   - The base directory which is used as a resolution starting point for the fragment paths.
    *   - One or more fragment paths specified without the .lore file extension.
    */
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      throw new RuntimeException("The compiler expects at least a base directory and a single fragment to compile.")
    }

    val baseDirectory = Path.of(args.head)
    val fragmentPaths = args.tail.toVector.map(path => Path.of(s"$path.lore"))
    val beforeCompile = System.nanoTime()
    val result = fromSources(baseDirectory, fragmentPaths: _*)
    val afterCompile = System.nanoTime()
    println(stringifyCompilationInfo(result.map(_._1)))
    result.map(_._2).foreach(writeResult(Path.of(args(0))))
    println(s"Compile time: ${((afterCompile - beforeCompile) / 1000) / 1000.0}ms.")
  }
}
