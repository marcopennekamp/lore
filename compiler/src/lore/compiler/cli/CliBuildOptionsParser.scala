package lore.compiler.cli

import lore.compiler.build.{BuildApi, BuildOptions, JsonBuildOptionsParser}
import lore.compiler.feedback.Feedback
import scopt.{OParser, Read}

import java.nio.file.{Files, Path}

object CliBuildOptionsParser {

  private implicit val pathReader: Read[Path] = Read.reads(Path.of(_))

  private val cliParserBuilder = OParser.builder[BuildOptions]

  private val cliParser = {
    import cliParserBuilder._
    OParser.sequence(
      programName("lore"),
      head("lore", "[experimental compiler]"),
      cmd("compile")
        .text("Compile a lore program with compilation options given via CLI arguments.")
        .children(
          arg[Path]("<source>...")
            .unbounded()
            .required()
            .action((path, options) => options.copy(sources = options.sources :+ path))
            .text("The source files to compile, relative to the current working directory. Specifying a directory will compile all files in the directory."),
          opt[Path]("sdk")
            .valueName("<directory>")
            .action((base, options) => options.copy(sdk = base))
            .text("The location of the Lore SDK. It must contain the Pyramid standard library in a directory called `pyramid/` and the runtime in a directory called `runtime/`."),
          opt[Path]('t', "target")
            .valueName("<file>")
            .action((target, options) => options.copy(target = target))
            .text("The file that the generated Javascript code is written to, the default being `lore-program.js`."),
          opt[Unit]("no-prettier")
            .action((_, options) => options.copy(enablePrettier = false))
            .text("Disable beautification of the generated Javascript code with prettier. This avoids an additional compile-time cost of a few hundred milliseconds."),
          opt[Unit]("runtime-logging")
            .action((_, options) => options.copy(compilerOptions = options.compilerOptions.copy(enableRuntimeLogging = true)))
            .text("Run-time logging generates various logging statements in the target Javascript code, which can greatly affect performance."),
          opt[Unit]("error-stack-traces")
            .action((_, options) => options.copy(compilerOptions = options.compilerOptions.copy(showFeedbackStackTraces = true)))
            .text("Report errors and warnings with stack traces. This allows you to trace the reported error back to a given place in the compiler's source code."),
        ),
      cmd("build")
        .action { (_, _) =>
          if (!Files.isRegularFile(BuildApi.buildFile)) {
            failure(s"Cannot build a lore program from the current directory. There is no `${BuildApi.buildFile}` build file.")
          }
          JsonBuildOptionsParser.parse(Files.readString(BuildApi.buildFile))
        }
        .text(s"Compile a lore program with compilation options taken from a `${BuildApi.buildFile}` build file in the current directory."),
      help("help").text("Prints this help text."),
    )
  }

  def parse(args: Array[String]): Option[BuildOptions] = OParser.parse(cliParser, args, BuildOptions())

}
