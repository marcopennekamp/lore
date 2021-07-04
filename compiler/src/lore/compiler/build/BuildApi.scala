package lore.compiler.build

import lore.compiler.LoreCompiler
import lore.compiler.core.{Compilation, CompilerOptions, Fragment, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.utils.CollectionExtensions.VectorExtension

import java.nio.file.{Files, Path}

/**
  * The Build API takes care of reading sources, reporting of errors and warnings, and writing the compilation result
  * to the target file.
  */
object BuildApi {

  def build(options: BuildOptions): Unit = {
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
  def compile(options: BuildOptions): Compilation[(Registry, String)] = {
    SdkDirectory.verify(options.sdk).flatMap { _ =>
      val sources = options.sources :+ options.sdk.resolve("pyramid")
      sources
        .map(SourceFiles.of)
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
  def writeResult(code: String)(implicit options: BuildOptions): Unit = {
    val targetPath = options.target
    val runtimePath = options.sdk.resolve(Path.of("runtime", "src", "lore", "runtime", "Lore.ts"))

    // To create the preamble, the runtime must be imported from the target file's point of reference. Relative Deno
    // paths must start with ./ or ../, so we additionally resolve the path starting from ./.
    val runtimeFromTargetPath = if (targetPath.getParent != null) {
      Path.of(".").resolve(targetPath.getParent.relativize(runtimePath))
    } else runtimePath
    val preamble = s"import Lore from '${runtimeFromTargetPath.toString}';\n\n"
    val output = preamble + code

    if (targetPath.getParent != null) {
      Files.createDirectories(targetPath.getParent)
    }
    Files.writeString(targetPath, output)

    if (options.enablePrettier) {
      Runtime.getRuntime.exec(s"prettier --write ${targetPath.toString}").waitFor()
    }
  }

}
