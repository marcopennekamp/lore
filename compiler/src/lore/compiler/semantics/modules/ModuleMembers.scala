package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException

import scala.annotation.nowarn
import scala.reflect.ClassTag

trait ModuleMembers[A <: BindingModuleMember]

object ModuleMembers {
  /**
    * Given `types` and `terms`, [[membersOfType]] returns the correctly typed [[ModuleMembers]] instance that contains
    * members of type `A`.
    */
  @nowarn
  def membersOfType[A <: BindingModuleMember, M[_ <: BindingModuleMember]](
    types: M[TypeModuleMember],
    terms: M[TermModuleMember],
  )(implicit memberTag: ClassTag[A]): M[A] = {
    // This implementation is incredibly hacky, but also kind of amazing. Pattern matching on `types` and `terms`
    // allows each case to correctly type the result, without any `asInstanceOf`, while the case guards ensure that the
    // module member is actually the same as the one in `types` or `terms`. The `@nowarn` is necessary to suppress a
    // warning for each match: `A` in `M[A]` isn't actually checkable at run time due to type erasure. That's what the
    // class tag is for, but Scala 2 doesn't put 2 and 2 together here. I don't blame the compiler for it, though. In
    // other words: Mum, look what type erasure did to me!
    types match {
      case members: M[A] if classOf[TypeModuleMember].isAssignableFrom(memberTag.runtimeClass) => members
      case _ => terms match {
        case members: M[A] if classOf[TermModuleMember].isAssignableFrom(memberTag.runtimeClass) => members
        case _ => throw CompilationException(s"Unknown kind of ModuleMember: ${memberTag.runtimeClass.getName}.")
      }
    }
  }
}
