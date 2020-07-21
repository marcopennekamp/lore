package lore.compiler.core

import fastparse.ParserInput

/**
  * A fragment with the given name and input.
  */
case class Fragment(name: String, input: ParserInput)
