package lore.compiler.build

import lore.compiler.LoreCompiler
import lore.compiler.assembly.AssembledFragment
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
  def compile(options: BuildOptions)(implicit reporter: Reporter): (Option[Registry], Vector[AssembledFragment]) = {
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

    // TODO (assembly): Turn this back to include all of Pyramid.
    //val sources = options.sources :+ options.sdk.resolve("pyramid")
    val sources = options.sources :+ options.sdk.resolve("pyramid").resolve("core2.lore")
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
  def writeResult(assembledFragment: AssembledFragment)(implicit options: BuildOptions): Unit = {
    val targetPath = options.target.resolve(assembledFragment.path)
    if (targetPath.getParent != null) {
      Files.createDirectories(targetPath.getParent)
    }

    Files.write(targetPath, assembledFragment.bytecode)
  }

}
