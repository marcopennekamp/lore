package lore.compiler.build

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}

import java.nio.file.{Files, Path}

object SdkDirectory {

  case class SdkNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK path `$path` does not exist or is not a directory."
  }

  case class PyramidNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK at `$path` does not contain the Pyramid standard library in a sub-directory `pyramid`."
  }

  def verify(sdkDirectory: Path)(implicit reporter: Reporter): Unit = {
    if (!Files.isDirectory(sdkDirectory)) {
      reporter.error(SdkNotFound(sdkDirectory))
    } else {
      def ensureDirectoryExists(directoryName: String, error: Path => Feedback.Error): Unit = {
        if (!Files.isDirectory(sdkDirectory.resolve(directoryName))) {
          reporter.error(error(sdkDirectory))
        }
      }

      ensureDirectoryExists("pyramid", PyramidNotFound)
    }
  }

}
