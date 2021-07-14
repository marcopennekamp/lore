package lore.compiler.utils

import scala.concurrent.ExecutionContext

case object ExecutionContexts {
  implicit val default: ExecutionContext = scala.concurrent.ExecutionContext.global
}
