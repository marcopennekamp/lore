package lore.compiler.core

import fastparse.ParserInput

import java.nio.file.Path

/**
  * A fragment with the given unique name, optionally a path, and input.
  *
  * The fragment's path is optional because fragments don't necessarily have to correspond to a path in the file
  * system, though this is the usual case.
  */
case class Fragment(name: String, path: Option[Path], input: ParserInput) {
  lazy val uri: Option[String] = path.map(_.toUri.toString)

  override def equals(other: Any): Boolean = other match {
    case other: Fragment => name == other.name
    case _ => false
  }
  override lazy val hashCode: Int = name.hashCode
}

object Fragment {
  def apply(name: String, input: ParserInput): Fragment = Fragment(name, None, input)
}
