package lore.parser

import fastparse.WhitespaceApi

trait IgnoreWhitespace {
  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  protected val whitespaceApi = White
}
