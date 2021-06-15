package lore.compiler

import lore.compiler.core._
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry

import java.nio.file.{Files, Path}

object Lore {

  def pyramid(baseDirectory: Path): Vector[Compilation[Fragment]] = Vector(
    fragment("pyramid.collections", baseDirectory.resolve(Path.of("pyramid", "collections.lore"))),
    fragment("pyramid.core", baseDirectory.resolve(Path.of("pyramid", "core.lore"))),
    fragment("pyramid.introspection", baseDirectory.resolve(Path.of("pyramid", "introspection.lore"))),
    fragment("pyramid.io",baseDirectory.resolve( Path.of("pyramid", "io.lore"))),
    fragment("pyramid.math", baseDirectory.resolve(Path.of("pyramid", "math.lore"))),
    fragment("pyramid.string", baseDirectory.resolve(Path.of("pyramid", "string.lore"))),
  )

  case class FragmentNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file '$path' does not exist."
  }

  def fragment(name: String, path: Path): Compilation[Fragment] = {
    import scala.jdk.CollectionConverters._

    if (Files.notExists(path)) {
      Compilation.fail(FragmentNotFound(path))
    } else {
      val source = Files.lines(path).iterator().asScala.mkString("\n") + "\n" // Ensure that the file ends in a newline.
      Compilation.succeed(Fragment(name, source))
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
  def fromSources(baseDirectory: Path, paths: Path*)(implicit options: CompilerOptions): Compilation[(Registry, String)] = {
    val allFragments = pyramid(baseDirectory) ++ paths.map(path => fragment(path.toString, baseDirectory.resolve(path))).toVector
    allFragments.simultaneous.flatMap { fragments =>
      val compiler = new LoreCompiler(fragments, options)
      compiler.compile()
    }
  }

  /**
    * Logs the compilation result in a user-palatable way, reporting errors or the successful result in text form.
    */
  def logCompilationResult(result: Compilation[Registry], compileTime: Double)(implicit options: CompilerOptions): Unit = {
    result match {
      case Errors(_, _) =>
        Feedback.loggerBlank.info("")
        Feedback.logger.error("Compilation failed with errors:")
        Feedback.logAll(result.feedback, options.showFeedbackStackTraces)
        Feedback.loggerBlank.error("")

      case Result(_, _) =>
        Feedback.loggerBlank.info("")
        Feedback.logger.info(s"Compilation was successful. (Total time: ${compileTime}ms)")
        Feedback.logAll(result.feedback, options.showFeedbackStackTraces)
        Feedback.loggerBlank.info("")
    }
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

    implicit val options: CompilerOptions = CompilerOptions(runtimeLogging = false)
    val baseDirectory = Path.of(args.head)
    val fragmentPaths = args.tail.toVector.map(path => Path.of(s"$path.lore"))
    val beforeCompile = System.nanoTime()
    val result = fromSources(baseDirectory, fragmentPaths: _*)
    val afterCompile = System.nanoTime()
    logCompilationResult(result.map(_._1), ((afterCompile - beforeCompile) / 1000) / 1000.0)
    result.map(_._2).foreach(writeResult(Path.of(args(0))))
  }

}
