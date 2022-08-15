package lore.compiler.core

import java.util.concurrent.atomic.AtomicLong

/**
  * A <i>temporary</i> unique key for local variables, parameters, and so on. A unique key is unique in the current
  * compiler run. It is used to uniquely identify local variables and parameters even when case classes are copied and
  * multiple local variable instances of the same parameter are created.
  *
  * Unique keys are open for other usages, but are currently only used by parameters and local variables.
  */
case class UniqueKey(id: Long) extends AnyVal

object UniqueKey {
  private val uniqueKeyCounter: AtomicLong = new AtomicLong()

  def fresh(): UniqueKey = UniqueKey(uniqueKeyCounter.getAndIncrement())
}

trait UniqueIdentifiable {
  def uniqueKey: UniqueKey

  override def equals(obj: Any): Boolean = obj match {
    case other: UniqueIdentifiable => this.uniqueKey == other.uniqueKey
    case _ => false
  }

  override def hashCode(): Int = uniqueKey.hashCode()
}
