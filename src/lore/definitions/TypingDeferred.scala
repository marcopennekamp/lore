package lore.definitions

import lore.compiler.C
import lore.types.Type

/**
  * As noted in the compiler specification, certain types cannot be resolved immediately, as they might reference
  * types that are registered after the current class type. Hence, we pass a `resolveType` function to the a
  * definition that has its typing deferred, which is then lazily evaluated to `tpe`.
  */
trait TypingDeferred[+T <: Type] { self: Definition =>
  protected def resolveType: () => C[T]

  // TODO: I am quite aware that this implementation results in two evaluations of resolveType. I don't think this
  //       will cause any issues, neither logical nor performance issues, but we might want to watch out for this
  //       anyway.

  /**
    * Verifies whether the given definition has a correct type. If not, all compilation errors are passed via the
    * returned compilation instance.
    *
    * This should be called before `tpe` is accessed!
    */
  def verifyType: C[Unit] = resolveType().map(_ => ()).associate(position.fragment)

  /**
    * The resolved and verified type of the definition.
    */
  lazy val tpe: T = TypingDeferred.assertVerified(resolveType())
}

object TypingDeferred {
  def assertVerified[T](compilation: C[T]): T = {
    compilation.getOrElse(
      throw new RuntimeException("A definition type could not be resolved. This should have been verified by DeclarationResolver.")
    )
  }
}
