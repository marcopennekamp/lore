package lore.compiler.feedback

import lore.compiler.core.{Fragment, Position}

import java.nio.file.Path

object BuildFeedback {

  case class DirectoryNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The directory `$path` does not exist."
  }

  case class FragmentNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file `$path` does not exist."
  }

  case class IllegalFileExtension(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file `$path` has an illegal file extension. It must either be a directory or" +
      s" end in `.lore`."
  }

  case class FileAccessFailed(path: Path, exception: Throwable) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file `$path` cannot be found or accessed due to an unknown error. The" +
      s" following exception occurred:\n$exception"
  }

  case class SdkNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK path `$path` does not exist or is not a directory."
  }

  case class PyramidNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The SDK at `$path` does not contain the Pyramid standard library in a" +
      s" sub-directory `pyramid`."
  }

  case class DuplicateFragmentName(fragment: Fragment) extends Feedback.Error(Position(fragment, 0, 0)) {
    override def message: String = s"The fragment `${fragment.name}` is defined multiple times. Fragments may not" +
      s" share names. Most likely you have specified a source file which is also included via a directory source, or" +
      s" multiple directory sources which point to the same file."
  }

}
