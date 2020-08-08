package lore.compiler.types

import lore.compiler.semantics.Registry
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.traverse._

object Ards {
  /**
    * Abstract-resolved direct subtypes: A set of direct subtypes that are resolved IF the given type is abstract.
    *
    * This is an implementation of the 'ards' function as defined in the spec. See the spec for more information.
    */
  def abstractResolvedDirectSubtypes(t: Type)(implicit registry: Registry): Set[Type] = {
    // TODO: How can we handle type variables?

    // TODO: Using Set like this (which is much slower than List) could be a major performance hog down the line.
    //       We should watch out for any performance problems stemming from ards evaluation.
    implicit val setMonad: Monad[Set] = new Monad[Set] {
      override def point[A](a: => A): Set[A] = Set(a)
      override def bind[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
    }
    def combinations(components: List[Set[Type]]) = components.sequence

    t match {
      case _ if !Type.isAbstract(t) => Set(t)
      case dt: DeclaredType => dt.directDeclaredSubtypes
      case ProductType(components) => combinations(components.map(abstractResolvedDirectSubtypes)).map(ProductType(_))
      case IntersectionType(types) => combinations(types.map(abstractResolvedDirectSubtypes).toList).map(IntersectionType.construct)
      case SumType(types) => types.flatMap(abstractResolvedDirectSubtypes)
      case BasicType.Any =>
        // TODO: Really? This should rather be the set of all types which have no supertype, i.e. direct descendants
        //       of Any. Or maybe, rather, let's not fuck with Any for abstract functions and return an error here.
        Set.empty
      case BasicType.Nothing => Set.empty
    }
  }
}
