package lore.compiler.assembly

import lore.compiler.poem.Poem

import java.util.concurrent.atomic.AtomicInteger

class RegisterProvider {
  private val counter: AtomicInteger = new AtomicInteger()

  /**
    * Provides a fresh, unique register ID.
    */
  def fresh(): Poem.Register = Poem.Register(counter.incrementAndGet())
}
