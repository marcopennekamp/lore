package lore.compiler.types

import lore.compiler.semantics.Registry
import scalaz.Monad
import scalaz.std.list._
import scalaz.syntax.traverse._

object Ards {
  /**
    * Abstract-resolved direct subtypes: A set of direct subtypes that are resolved IF the given type is abstract.
    */
  def abstractResolvedDirectSubtypes(t: Type)(implicit registry: Registry): Set[Type] = {
    // TODO: How can we handle type variables?

    // TODO: Using Set like this (which is much slower than a Vector) could be a major performance hog down the line.
    //       We should watch out for any performance problems stemming from ards evaluation.
    implicit val setMonad: Monad[Set] = new Monad[Set] {
      override def point[A](a: => A): Set[A] = Set(a)
      override def bind[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
    }
    def combinations(components: List[Set[Type]]) = components.sequence

    // TODO: For all traits A, B, C, ... which extend a component type +T, shouldn't +T have A, B, C, ... as direct
    //       declared subtypes? If we declare an abstract function over +T, we would have to check that all traits
    //       and structs containing a component of type T satisfy the totality constraint.
    //       I don't think that's true. To satisfy the abstract function, we actually just have to make sure that all
    //       component-ish subtypes of +T satisfy the totality constraint. So if abstract T has two subtypes T1 and
    //       T2, if we implement concrete functions for T1 and T2, we can be sure that any entity that has a component
    //       of type T (which cannot be instantiated itself because it is abstract) can dispatch to either +T1 or +T2,
    //       because those are the direct options. While A would be a subtype of +T if it extends +T, it is not a
    //       direct subtype in the sense of this algorithm, just as a specialization +T & L for some label L wouldn't
    //       be a direct subtype either.
    t match {
      case _ if !Type.isAbstract(t) => Set(t)
      case dt: DeclaredType => registry.declaredTypeHierarchy.getDirectSubtypes(dt).toSet
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
