package lore.compiler.types

import lore.compiler.core.Compilation.{C, Verification}

/**
  * As noted in the compiler specification, certain types cannot be resolved immediately, as they might reference
  * types that are registered after the current class type. Hence, we pass a `typeResolver` function to a definition
  * or type that has its typing deferred, which is then lazily evaluated to `tpe`.
  */
trait TypingDeferred[+T <: Type] {
  /**
    * The function that resolves the type whose typing has been deferred.
    */
  protected def typeResolver: () => C[T]

  /**
    * The type resolved by the type resolver.
    */
  private lazy val resolvedType: C[T] = typeResolver()

  /**
    * Verifies whether the type is resolved correctly. If not, all compilation errors are passed via the
    * returned compilation instance.
    *
    * This should be called before `tpe` is accessed!
    */
  def verifyDeferredTyping: Verification = resolvedType.map(_ => ())

  /**
    * The resolved and verified type.
    */
  lazy val tpe: T = resolvedType.getOrElse(
    throw new RuntimeException("A deferred type could not be resolved. This should have been verified in DeclarationResolver.")
  )
}
