package lore.compiler.build

import lore.compiler.LoreCompiler
import lore.compiler.core.{CompilationException, CompilerOptions, Fragment, Position}
import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.semantics.Registry

import java.nio.file.{Files, Path}

/**
  * The Build API takes care of reading sources, reporting of errors and warnings, and writing the compilation result
  * to the target file.
  */
object BuildApi {

  val buildFile: Path = Path.of("lore.build.json")

  def build(options: BuildOptions): Unit = {
    implicit val reporter: MemoReporter = MemoReporter()

    val compilationStartTime = System.nanoTime()
    val (_, optionalCode) = compile(options)
    val compilationEndTime = System.nanoTime()

    logCompilationFeedback(reporter, compilationStartTime, compilationEndTime)(options.compilerOptions)
    optionalCode.foreach(writeResult(_)(options))
  }

  /**
    * Compiles a Lore program from the given build options.
    */
  def compile(options: BuildOptions)(implicit reporter: Reporter): (Option[Registry], Option[String]) = {
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

  case class DuplicateFragmentName(fragment: Fragment) extends Feedback.Error(Position(fragment, 0, 0)) {
    override def message: String = s"The fragment '${fragment.name}' is defined multiple times. Fragments may not " +
      s"share names. Most likely you have specified a source file which is also included via a directory source, or " +
      s"multiple directory sources which point to the same file."
  }

  /**
    * Gets all fragments that can be found given the build options.
    */
  def getFragments(options: BuildOptions)(implicit reporter: Reporter): Vector[Fragment] = {
    SdkDirectory.verify(options.sdk)

    val sources = options.sources :+ options.sdk.resolve("pyramid")
    sources
      .flatMap(SourceFiles.of)
      .filterDuplicates(_.name, DuplicateFragmentName)
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
