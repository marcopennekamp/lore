package lore.compiler.cli

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback

import java.nio.file.{Files, Path}

object BaseDirectory {

  case class BaseDirectoryNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The base directory `$path` does not exist."
  }

  case class BaseDirectoryDirectoryExpected(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The base directory `$path` is not a directory."
  }

  def verify(baseDirectory: Path): Verification = {
    if (!Files.exists(baseDirectory)) {
      Compilation.fail(BaseDirectoryNotFound(baseDirectory))
    } else if (!Files.isDirectory(baseDirectory)) {
      Compilation.fail(BaseDirectoryDirectoryExpected(baseDirectory))
    } else Verification.succeed
  }

}
