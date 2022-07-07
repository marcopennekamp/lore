package lore.compiler.build

import lore.compiler.core.Fragment
import lore.compiler.feedback.{BuildFeedback, Reporter}

import java.nio.file.{FileSystems, Files, Path, PathMatcher}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Failure, Success, Try}

object SourceFiles {

  private val lorePathMatcher: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.lore")

  /**
    * Returns all fragments described by the given path. This may either be a file ending in `.lore` or a directory
    * containing `.lore` files.
    *
    * We require all Lore sources to end in `.lore` for consistency with how Lore sources are found when directories
    * are specified.
    */
  def of(path: Path)(implicit reporter: Reporter): Vector[Fragment] = {
    if (lorePathMatcher.matches(path)) {
      if (Files.exists(path)) {
        ofFile(path).toVector
      } else {
        reporter.error(BuildFeedback.FragmentNotFound(path))
        Vector.empty
      }
    } else if (Files.isDirectory(path)) {
      ofDirectory(path)
    } else {
      if (path.getFileName.toString.contains('.')) {
        reporter.error(BuildFeedback.IllegalFileExtension(path))
        Vector.empty
      } else {
        reporter.error(BuildFeedback.DirectoryNotFound(path))
        Vector.empty
      }
    }
  }

  private def ofDirectory(directory: Path)(implicit reporter: Reporter): Vector[Fragment] = {
    Files.walk(directory)
      .filter(Files.isRegularFile(_))
      .filter(lorePathMatcher.matches(_))
      .iterator().asScala.toVector
      .flatMap(ofFile)
  }

  def ofFile(path: Path)(implicit reporter: Reporter): Option[Fragment] = {
    // The additional newline ensures that the file ends in a newline.
    Try(Files.readString(path) + "\n") match {
      case Success(source) => Some(Fragment(path.toString, Some(path), source))
      case Failure(exception) =>
        reporter.error(BuildFeedback.FileAccessFailed(path, exception))
        None
    }
  }

}
