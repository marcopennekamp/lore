package lore.compiler.semantics.members

import lore.compiler.core.CompilationException
import lore.compiler.types.Type

import scala.util.hashing.MurmurHash3

/**
  * A member of a given type. This can either be a declared member (such as a property) or an implicitly accessible
  * member.
  *
  * @param isAssignable Whether the member can be assigned a new value with an assignment expression.
  * @param isMutable Whether the member's value can change during the course of program execution. Rarely, a member may
  *                  not be assignable but still mutable: if we have an intersection type where parts A and B each have
  *                  a member X, but only A's member is assignable, we cannot allow the combined member to be
  *                  assignable, but A's member may still be reassigned, thereby also changing the value of this
  *                  combined member. (They "point" to the same property behind all the type facade.)
  */
case class Member(
  name: String,
  tpe: Type,
  isAssignable: Boolean = false,
  isMutable: Boolean = false,
) {
  if (isAssignable && !isMutable) {
    throw CompilationException(s"An assignable member cannot be immutable. (name: $name, type: $tpe)")
  }

  override def equals(obj: Any): Boolean = obj match {
    case Member(name2, tpe2, _, _) => name == name2 && tpe == tpe2
    case _ => false
  }

  override def hashCode(): Int = MurmurHash3.productHash((name, tpe))
}
