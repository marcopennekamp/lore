package lore.compiler.assembly

import lore.compiler.poem.Poem

import java.util.concurrent.atomic.AtomicInteger

class RegisterProvider {
  private val counter: AtomicInteger = new AtomicInteger(0)

  /**
    * Provides a fresh, unique register ID. The first ID is guaranteed to be 0.
    */
  def fresh(): Poem.Register = Poem.Register(counter.getAndIncrement())
}
