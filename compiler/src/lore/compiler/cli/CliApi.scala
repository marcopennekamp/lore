package lore.compiler.cli

import lore.compiler.LoreCompiler
import lore.compiler.core._
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.utils.CollectionExtensions.VectorExtension

import java.nio.file.{Files, Path}

/**
  * The CliApi takes care of parsing CLI options, file input/output, and standard command-line reporting of errors and
  * warnings.
  */
object CliApi {

  def main(args: Array[String]): Unit = {
    CliOptionsParser.parse(args) match {
      case Some(options) => compileAndWrite(options)
      case None => // scopt will already have written an error message to the console.
    }
  }

  private def compileAndWrite(options: CliOptions): Unit = {
    val compilationStartTime = System.nanoTime()
    val result = compile(options)
    val compilationEndTime = System.nanoTime()

    logCompilationResult(result, compilationStartTime, compilationEndTime)(options.compilerOptions)
    result.foreach {
      case (_, code) => writeResult(code)(options)
    }
  }

  case class DuplicateFragmentName(fragment: Fragment) extends Feedback.Error(Position(fragment, 0)) {
    override def message: String = s"The fragment '${fragment.name}' is defined multiple times. Fragments may not " +
      s"share names. Most likely you have specified a source file which is also included via a directory source, or " +
      s"multiple directory sources which point to the same file."
  }

  /**
    * Compiles a Lore program from the given CLI options.
    */
  def compile(options: CliOptions): Compilation[(Registry, String)] = {
    BaseDirectory.verify(options.baseDirectory).flatMap { _ =>
      options.sources
        .map(options.baseDirectory.resolve)
        .map(SourceFiles.of(_)(options))
        .simultaneous
        .map(_.flatten)
        .flatMap(_.requireUnique(_.name, DuplicateFragmentName))
        .flatMap(fragments => LoreCompiler.compile(fragments, options.compilerOptions))
    }
  }

  /**
    * Logs the compilation result in a user-palatable way, reporting errors or the successful result in text form.
    */
  def logCompilationResult(result: Compilation[Any], compilationStartTime: Long, compilationEndTime: Long)(implicit options: CompilerOptions): Unit = {
    val compilationTime = ((compilationEndTime - compilationStartTime) / 1000) / 1000.0
    result match {
      case Compilation.Success(_, _) =>
        Feedback.loggerBlank.info("")
        Feedback.logger.info(s"Compilation was successful. (Total time: ${compilationTime}ms)")
        Feedback.logAll(result.feedback, options.showFeedbackStackTraces)
        Feedback.loggerBlank.info("")

      case Compilation.Failure(_, _) =>
        Feedback.loggerBlank.info("")
        Feedback.logger.error("Compilation failed with errors:")
        Feedback.logAll(result.feedback, options.showFeedbackStackTraces)
        Feedback.loggerBlank.error("")
    }
  }

  /**
    * Writes the result of the compilation to the file system.
    */
  def writeResult(code: String)(implicit options: CliOptions): Unit = {
    val outputPath = options.baseDirectory.resolve(options.outputFile)
    val runtimePath = options.baseDirectory.resolve(Path.of("runtime", "src", "lore", "runtime", "Lore.ts"))

    // To create the preamble, the runtime must be imported from the output file's point of reference. Relative Deno
    // paths must start with ./ or ../, so we additionally resolve the path starting from ./.
    val runtimeFromOutputPath = outputPath.getParent.relativize(runtimePath)
    val preamble = s"import Lore from './${runtimeFromOutputPath.toString}';\n\n"
    val output = preamble + code

    Files.createDirectories(outputPath.getParent)
    Files.writeString(outputPath, output)
    Runtime.getRuntime.exec(s"prettier --write ${outputPath.toString}")
  }

}
