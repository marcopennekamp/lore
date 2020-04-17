package lore.types

import lore.compiler.C
import lore.compiler.Compilation.Verification

/**
  * As noted in the compiler specification, certain types cannot be resolved immediately, as they might reference
  * types that are registered after the current class type. Hence, we pass a `resolveType` function to a definition
  * or type that has its typing deferred, which is then lazily evaluated to `tpe`.
  */
trait TypingDeferred[+T <: Type] {
  protected def resolveType: () => C[T]

  // TODO: I am quite aware that this implementation results in two evaluations of resolveType. I don't think this
  //       will cause any issues, neither logical nor performance issues, but we might want to watch out for this
  //       anyway.

  /**
    * Verifies whether the resolved type is correct. If not, all compilation errors are passed via the
    * returned compilation instance.
    *
    * This should be called before `tpe` is accessed!
    */
  def verifyType: Verification = resolveType().map(_ => ())

  /**
    * The resolved and verified type.
    */
  lazy val tpe: T = TypingDeferred.assertVerified(resolveType())
}

object TypingDeferred {
  def assertVerified[T](compilation: C[T]): T = {
    compilation.getOrElse(
      throw new RuntimeException("A deferred type could not be resolved. This should have been verified in DeclarationResolver.")
    )
  }
}
