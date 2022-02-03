package lore.compiler.semantics.scopes

import lore.compiler.target.Target
import lore.compiler.transpilation.RuntimeNames
import lore.compiler.types.Type

import java.util.concurrent.atomic.AtomicLong

/**
  * A LocalVariable is a variable declared in a local scope.
  *
  * The unique key is used to differentiate between local variables defined in nested local scopes. The unique key is
  * global to the current compiler run.
  *
  * Example:
  *
  * {{{
  * func foo(): Int = do
  *   let x = 7
  *   let mut y = 0
  *   if x > 5
  *     let x = 2
  *     y = x
  *   end
  * end
  * }}}
  *
  * Both `x` variables have the same type `Int` and are immutable. Hence, as LocalVariables without a unique key they
  * would be equal. The unique key allows the assembly phase to differentiate between these two variables. For example,
  * the first `x` would receive key 0, `y` key 1, and the second `x` key 2.
  */
case class LocalVariable(
  uniqueKey: LocalVariable.UniqueKey,
  name: String,
  tpe: Type,
  override val isMutable: Boolean,
) extends TypedBinding {
  lazy val targetVariable: Target.Variable = RuntimeNames.localVariable(name)
}

object LocalVariable {
  case class UniqueKey(id: Long) extends AnyVal

  private val uniqueKeyCounter: AtomicLong = new AtomicLong()

  def apply(name: String, tpe: Type, isMutable: Boolean): LocalVariable = {
    LocalVariable(UniqueKey(uniqueKeyCounter.getAndIncrement()), name, tpe, isMutable)
  }

  /**
    * Creates an <b>immutable</b> local variable.
    */
  def apply(name: String, tpe: Type): LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
