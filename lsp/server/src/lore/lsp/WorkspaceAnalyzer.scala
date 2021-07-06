package lore.lsp

import lore.compiler.build.{BuildApi, JsonBuildOptionsParser}
import lore.compiler.core.Compilation
import lore.compiler.semantics.Registry
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.util.{Failure, Success, Using}

object WorkspaceAnalyzer {

  def analyze()(implicit client: LanguageClient): Compilation[Registry] = {
    val buildOptions = JsonBuildOptionsParser.parse(Files.readString(BuildApi.buildFile))
    MessageLogger.info(buildOptions.toString)

    // Before we invoke the compiler, we have to redirect any of its stdout messages, because LSP communicates over
    // stdout and we don't want the compiler to interfere with log messages.
    val stdout = System.out
    val stream = new ByteArrayOutputStream
    System.setOut(new PrintStream(stream, true, StandardCharsets.UTF_8))

    val result = Using(new PrintStream(stream, true, StandardCharsets.UTF_8)) { printStream =>
      System.setOut(printStream)
      BuildApi.analyze(buildOptions)
    }

    // Make sure that stdout is set back to System.out so that we can communicate with the client again.
    System.setOut(stdout)

    val compilation = result match {
      case Failure(exception) =>
        MessageLogger.info(s"Compilation failed with an exception:\n$exception")
        throw exception

      case Success(value) => value
    }

    val compilationLog = stream.toString(StandardCharsets.UTF_8)
    MessageLogger.info(s"Compilation log:\n$compilationLog")

    compilation
  }

}
