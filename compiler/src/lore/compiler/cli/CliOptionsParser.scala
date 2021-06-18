package lore.compiler.cli

import scopt.{OParser, Read}

import java.nio.file.Path

object CliOptionsParser {

  private implicit val pathReader: Read[Path] = Read.reads(Path.of(_))

  private val cliParserBuilder = OParser.builder[CliOptions]

  private val cliParser = {
    import cliParserBuilder._
    OParser.sequence(
      programName("lore"),
      head("lore", "[experimental compiler]"),
      arg[Path]("<source>...")
        .unbounded()
        .required()
        .action((path, options) => options.copy(sources = options.sources :+ path))
        .text("The source files to compile. Specifying a directory will compile all files in the directory."),
      opt[Path]('b', "base-directory")
        .valueName("<directory>")
        .action((base, options) => options.copy(baseDirectory = base))
        .text("The base directory acts as the root for all files accessed by the compiler. It must contain the Pyramid standard library in a directory called `pyramid/`."),
      opt[Path]('o', "out")
        .valueName("<file>")
        .action((out, options) => options.copy(outputFile = out))
        .text("The file that the generated Javascript code is written to, the default being `lore-program.js`."),
      opt[Unit]("runtime-logging")
        .action((_, options) => options.copy(compilerOptions = options.compilerOptions.copy(enableRuntimeLogging = true)))
        .text("Run-time logging generates various logging statements in the target Javascript code, which can greatly affect performance."),
      opt[Unit]("error-stack-traces")
        .action((_, options) => options.copy(compilerOptions = options.compilerOptions.copy(showFeedbackStackTraces = true)))
        .text("Report errors and warnings with stack traces. This allows you to trace the reported error back to a given place in the compiler's source code."),
      help("help").text("Prints this help text."),
    )
  }

  def parse(args: Array[String]): Option[CliOptions] = OParser.parse(cliParser, args, CliOptions())

}
