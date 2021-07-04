package lore.compiler.build

import lore.compiler.core.{Compilation, Fragment, Position}
import lore.compiler.feedback.Feedback

import java.nio.file.{FileSystems, Files, Path, PathMatcher}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Failure, Success, Using}

object SourceFiles {

  case class DirectoryNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The directory '$path' does not exist."
  }

  case class FragmentNotFound(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file '$path' does not exist."
  }

  case class IllegalFileExtension(path: Path) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file '$path' has an illegal file extension. It must either be a directory or end in '.lore'."
  }

  case class FileAccessFailed(path: Path, exception: Throwable) extends Feedback.Error(Position.unknown) {
    override def message: String = s"The file '$path' cannot be found or accessed due to an unknown error. The following exception occurred:\n$exception"
  }

  private val lorePathMatcher: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.lore")

  /**
    * Returns all fragments described by the given path. This may either be a file ending in `.lore` or a directory
    * containing `.lore` files.
    *
    * We require all Lore sources to end in `.lore` for consistency with how Lore sources are found when directories
    * are specified.
    */
  def of(path: Path): Compilation[Vector[Fragment]] = {
    if (lorePathMatcher.matches(path)) {
      if (Files.exists(path)) {
        ofFile(path).map(Vector(_))
      } else {
        Compilation.fail(FragmentNotFound(path))
      }
    } else if (Files.isDirectory(path)) {
      ofDirectory(path)
    } else {
      if (path.getFileName.toString.contains('.')) {
        Compilation.fail(IllegalFileExtension(path))
      } else {
        Compilation.fail(DirectoryNotFound(path))
      }
    }
  }

  private def ofDirectory(directory: Path): Compilation[Vector[Fragment]] = {
    Files.walk(directory)
      .filter(Files.isRegularFile(_))
      .filter(lorePathMatcher.matches(_))
      .map(ofFile)
      .iterator().asScala.toVector.simultaneous
  }

  private def ofFile(path: Path): Compilation[Fragment] = {
    // The additional newline ensures that the file ends in a newline.
    Using(Files.lines(path))(_.iterator().asScala.mkString("\n") + "\n") match {
      case Success(source) =>
        val name = path.toString
        Compilation.succeed(Fragment(name, source))
      case Failure(exception) => Compilation.fail(FileAccessFailed(path, exception))
    }
  }

}
