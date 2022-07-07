package lore.compiler.build

import lore.compiler.LoreCompiler
import lore.compiler.core.{CompilationException, CompilerOptions, Fragment}
import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{BuildFeedback, Feedback, MemoReporter, Reporter}
import lore.compiler.semantics.Registry

import java.nio.file.{Files, Path}

/**
  * The Build API takes care of reading sources, reporting of errors and warnings, and writing the compilation result
  * to the target file.
  */
object BuildApi {

  val buildFile: Path = Path.of("lore.build.json")

  /**
    * Builds the Lore program according to `options` and returns whether the compilation was successful.
    */
  def build(options: BuildOptions): Boolean = {
    implicit val reporter: MemoReporter = MemoReporter()

    val compilationStartTime = System.nanoTime()
    val (_, bytecode) = compile(options)
    bytecode.foreach(writeResult(_)(options))
    val compilationEndTime = System.nanoTime()

    logCompilationFeedback(reporter, compilationStartTime, compilationEndTime)(options.compilerOptions)
    !reporter.hasErrors
  }

  /**
    * Compiles a Lore program from the given build options.
    */
  def compile(options: BuildOptions)(implicit reporter: Reporter): (Option[Registry], Option[Array[Byte]]) = {
    val fragments = getFragments(options)
    LoreCompiler.compile(fragments, options.compilerOptions)
  }

  /**
    * Analyzes a Lore program from the given build options.
    *
    * This is used by the language server, so by default it never terminates compilation early.
    */
  def analyze(options: BuildOptions)(implicit reporter: Reporter): Registry = {
    val fragments = getFragments(options)
    LoreCompiler.analyze(fragments, exitEarly = false).getOrElse(
      throw CompilationException("`LoreCompiler.analyze` called with `exitEarly = false` should always return a Registry.")
    )
  }

  def analyzeExitEarly(options: BuildOptions)(implicit reporter: Reporter): Option[Registry] = {
    val fragments = getFragments(options)
    LoreCompiler.analyze(fragments, exitEarly = true)
  }

  /**
    * Gets all fragments that can be found given the build options.
    */
  def getFragments(options: BuildOptions)(implicit reporter: Reporter): Vector[Fragment] = {
    SdkDirectory.verify(options.sdk)

    val sources = options.sources :+ options.sdk.resolve("pyramid")
    sources
      .flatMap(SourceFiles.of)
      .filterDuplicates(_.name, BuildFeedback.DuplicateFragmentName)
  }

  /**
    * Logs the compilation feedback in a user-palatable way.
    */
  def logCompilationFeedback(reporter: MemoReporter, compilationStartTime: Long, compilationEndTime: Long)(implicit options: CompilerOptions): Unit = {
    val compilationTime = ((compilationEndTime - compilationStartTime) / 1000) / 1000.0

    if (!reporter.hasErrors) {
      Feedback.loggerBlank.info("")
      Feedback.logger.info(s"Compilation was successful. (Total time: ${compilationTime}ms)")
      Feedback.logAll(reporter.feedback, options.showFeedbackStackTraces)
      Feedback.loggerBlank.info("")
    } else {
      Feedback.loggerBlank.info("")
      Feedback.logger.error("Compilation failed with errors:")
      Feedback.logAll(reporter.feedback, options.showFeedbackStackTraces)
      Feedback.loggerBlank.error("")
    }
  }

  /**
    * Writes the result of the compilation to the file system.
    */
  def writeResult(bytecode: Array[Byte])(implicit options: BuildOptions): Unit = {
    val targetPath = options.target
    if (targetPath.getParent != null) {
      Files.createDirectories(targetPath.getParent)
    }
    Files.write(targetPath, bytecode)
  }

}
