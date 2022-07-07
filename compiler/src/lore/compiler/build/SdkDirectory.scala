package lore.compiler.build

import lore.compiler.feedback.{BuildFeedback, Feedback, Reporter}

import java.nio.file.{Files, Path}

object SdkDirectory {

  def verify(sdkDirectory: Path)(implicit reporter: Reporter): Unit = {
    if (!Files.isDirectory(sdkDirectory)) {
      reporter.error(BuildFeedback.SdkNotFound(sdkDirectory))
    } else {
      def ensureDirectoryExists(directoryName: String, error: Path => Feedback.Error): Unit = {
        if (!Files.isDirectory(sdkDirectory.resolve(directoryName))) {
          reporter.error(error(sdkDirectory))
        }
      }

      ensureDirectoryExists("pyramid", BuildFeedback.PyramidNotFound)
    }
  }

}
