package lore.compiler.build

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback

import java.nio.file.{Files, Path}

object SdkDirectory {

  case class SdkNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK path `$path` does not exist or is not a directory."
  }

  case class PyramidNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK at `$path` does not contain the Pyramid standard library in a sub-directory `pyramid`."
  }

  case class RuntimeNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK at `$path` does not contain the runtime in a sub-directory `runtime`."
  }

  def verify(sdkDirectory: Path): Verification = {
    if (!Files.isDirectory(sdkDirectory)) {
      Compilation.fail(SdkNotFound(sdkDirectory))
    } else {
      val pyramid = sdkDirectory.resolve("pyramid")
      val runtime = sdkDirectory.resolve("runtime")
      (
        if (!Files.isDirectory(pyramid)) Compilation.fail(PyramidNotFound(sdkDirectory)) else Verification.succeed,
        if (!Files.isDirectory(runtime)) Compilation.fail(RuntimeNotFound(sdkDirectory)) else Verification.succeed,
        ).simultaneous.verification
    }
  }

}
