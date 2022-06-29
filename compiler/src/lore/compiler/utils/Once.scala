package lore.compiler.utils

/**
  * A value that is assignable exactly once and must be assigned before it can be used.
  */
class Once[A] {
  private[this] var _value: A = _

  def isAssigned: Boolean = _value != null

  def assign(value: A): Unit = {
    if (isAssigned) {
      throw new IllegalStateException(s"The value has already been assigned!")
    }
    _value = value
  }

  def value: A = {
    if (!isAssigned) {
      throw new IllegalStateException(s"The value has not yet been assigned!")
    }
    _value
  }

  def toOption: Option[A] = if (isAssigned) Some(_value) else None
}

object Once {
  implicit def implicitValue[A](once: Once[A]): A = once.value

  def apply[A](value: A): Once[A] = {
    val once = new Once[A]
    once.assign(value)
    once
  }
}
