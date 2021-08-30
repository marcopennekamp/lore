package lore.compiler.transpilation.functions

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types._

/**
  * Collects properties of the multi-function that are relevant for its transpilation.
  */
case class MultiFunctionProperties(mf: MultiFunctionDefinition) {

  /**
    * Whether this multi-function consists of a single function.
    */
  lazy val isSingleFunction: Boolean = mf.functions.length == 1

  /**
    * All possible arities of the functions.
    */
  lazy val arities: Set[Int] = mf.functions.map(_.signature.arity).toSet

  /**
    * If the multi-function only has functions of the same arity, this option contains that arity.
    */
  lazy val uniqueArity: Option[Int] = if (arities.size == 1) arities.headOption else None

  /**
    * Whether we can bypass creating tuple types around argument types on both the left and right side of fits. If
    * there is only a single argument across the board, tuple types are useless overhead. We can easily optimize this
    * overhead away.
    */
  lazy val mayUnpackArgumentTuple: Boolean = uniqueArity.contains(1)

  /**
    * This heuristic decides whether the dispatch cache should be used for this multi-function or not. At some
    * complexity threshold of the parameter types of all functions, we use the dispatch cache. This threshold is
    * reached relatively early, so only the simplest functions won't use the dispatch cache.
    */
  lazy val shouldUseDispatchCache: Boolean = {
    // The estimated run-time complexity of checking a subtyping relationship when the type in question is on the
    // right-hand side.
    def typeComplexity(tpe: Type): Int = tpe match {
      case tv: TypeVariable => 10 + typeComplexity(tv.lowerBound) + typeComplexity(tv.upperBound)
      case SumType(parts) => 1 + parts.map(typeComplexity).sum
      case IntersectionType(parts) => 1 + parts.map(typeComplexity).sum
      case TupleType(elements) => 1 + elements.map(typeComplexity).sum
      case ListType(element) => 1 + typeComplexity(element)
      case MapType(key, value) => 1 + typeComplexity(key) + typeComplexity(value)
      case _: TraitType =>
        // Checking whether a trait is a supertype may be inherently expensive because we have to walk the left
        // type's supertype hierarchy.
        5
      case ShapeType(properties) => 3 + properties.map(_._2.tpe).map(typeComplexity).sum * 2
      case _ => 1
    }

    def signatureComplexity(function: FunctionDefinition): Int = function.signature.parameters.map(_.tpe).map(typeComplexity).sum

    mf.functions.map(signatureComplexity).sum >= 5
  }

}
