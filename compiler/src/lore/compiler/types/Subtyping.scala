package lore.compiler.types

object Subtyping {

  /**
    * Whether t1 is a subtype of t2.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = t1 == t2 || rules.exists(rule => rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2)))

  /**
    * Whether t1 is a supertype of t2.
    */
  def isSupertype(t1: Type, t2: Type): Boolean = isSubtype(t2, t1)

  /**
    * Whether t1 is a strict subtype of t2.
    */
  def isStrictSubtype(t1: Type, t2: Type): Boolean = t1 != t2 && isSubtype(t1, t2)

  /**
    * Whether t1 is a strict supertype of t2.
    */
  def isStrictSupertype(t1: Type, t2: Type): Boolean = t1 != t2 && isSupertype(t1, t2)

  /**
    * We define the calculation of the subtyping relation in terms of rules that possibly match a pair of types
    * and then decide whether these types are in the relation or not. We define rules as partial functions
    * so that we can support preconditions.
    */
  private type Rule = PartialFunction[(Type, Type), Boolean]

  private val rules: Vector[Rule] = Vector(
    // All instances of v1 are definitely subtypes of t2 if v1's upper bound is a subtype of t2, hence ensuring
    // that any instance of v1 has t2 as a supertype.
    { case (v1: TypeVariable, t2) => isSubtype(v1.upperBound, t2) },
    // t1 is definitely a subtype of all instances of v2 if v2's lower bound ensures that instances of v2 are always
    // a supertype of t1.
    { case (t1, v2: TypeVariable) => isSubtype(t1, v2.lowerBound) },

    // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal d2.
    // It suffices to check for declared supertypes, since a shape supertype can never be a subtype of a declared type.
    // TODO: Once we have introduced covariant (and possibly contravariant) traits/structs, we will additionally have
    //       to check whether d1's typeArguments are a subtype of d2's type arguments.
    // TODO: Once we introduce parametric declared types, we might have to move this rule out of here.
    // TODO: Now that we have "multiple inheritance", checking for a supertype has gone from being linear to possibly
    //       a broad search up the hierarchy graph. There is a pretty simple way to make this more performant: Cache
    //       all possible supertypes in each declared type, then use a hashed map to allow a quick lookup. Because
    //       what we essentially have here is the question: Is d2 in the supertype map of d1? This optimization is
    //       especially potent for the runtime, where we will have to figure out how to solve the subtyping question
    //       for declared types in a performant manner!
    { case (d1: DeclaredType, d2: DeclaredType) => d1 == d2 || d1.declaredSupertypes.exists(isSubtype(_, d2)) },

    // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
    { case (i1: IntersectionType, i2: IntersectionType) => i2.parts.forall(ic2 => isAnyPartSubtypeOf(i1, ic2)) },
    // An intersection type i1 is the subtype of another type t2 if one part of i1 is a subtype of t2.
    { case (i1: IntersectionType, t2) => isAnyPartSubtypeOf(i1, t2) },
    // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
    { case (t1, i2: IntersectionType) => i2.parts.forall(ic2 => isSubtype(t1, ic2)) },

    // TODO: The rule fails for nested sum types: (A | B) | C </= A | B | C. In general, it seems we have to look at
    //       all possible subsets of s2 to generally decide this rule. This would be detrimental to run-time performance,
    //       however, so we should see how this interacts with our sum type "normal form". I think if we keep sum types
    //       in a flattened normal form, this sort of piece-by-piece comparison should yield the correct results.
    // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
    { case (s1: SumType, s2: SumType) => s1.parts.forall(sc1 => s2.parts.exists(sc2 => isSubtype(sc1, sc2))) },
    // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
    { case (t1, s2: SumType) => s2.parts.exists(sc2 => isSubtype(t1, sc2)) },
    // A sum type s1 is the subtype of a non-sum type t2, if all individual types in s1 are subtypes of t2.
    // More formally: A <= C and B <= C implies A | B <= C
    { case (s1: SumType, t2) => s1.parts.forall(sc1 => isSubtype(sc1, t2)) },

    // A product type p1 is the subtype of a product type p2 if both types have the same number of elements
    // and each element of p1 is a subtype of the element in p2 that is at the same position.
    {
      case (p1: ProductType, p2: ProductType) =>
        p1.elements.size == p2.elements.size && p1.elements.zip(p2.elements).forall {
          case (e1, e2) => isSubtype(e1, e2)
        }
    },

    // Functions are contravariant in their input and covariant in their output.
    { case (f1: FunctionType, f2: FunctionType) => isSubtype(f2.input, f1.input) && isSubtype(f1.output, f2.output) },

    // Lists are covariant.
    { case (l1: ListType, l2: ListType) => isSubtype(l1.element, l2.element) },

    // Maps are invariant because they are mutable (for now).
    { case (m1: MapType, m2: MapType) => m1.key == m2.key && m1.value == m2.value },

    // A shape type s1 is a subtype of another shape type s2 if all properties p2 in s2 are contained (by name) in s1
    // as p1. Each type of p1 must be a subtype of the corresponding p2's type.
    {
      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).forall {
          case (p2, Some(p1)) => isSubtype(p1.tpe, p2.tpe)
          case _ => false
        }
    },
    // Declared type/shape subtyping can be delegated to shape/shape subtyping by viewing the declared type as a shape
    // type.
    { case (d1: DeclaredType, s2: ShapeType) => isSubtype(d1.asShapeType, s2) },

    // Handle basic types. Int is a subtype of Real.
    { case (a: BasicType, b: BasicType) => a eq b },
    { case (BasicType.Int, BasicType.Real) => true },

    // The Any type is supertype of all types.
    { case (_, BasicType.Any) => true },

    // The Nothing type is subtype of all types.
    { case (BasicType.Nothing, _) => true },
  )

  private def isAnyPartSubtypeOf(intersectionType: IntersectionType, candidateSupertype: Type): Boolean = {
    intersectionType.parts.exists(t => isSubtype(t, candidateSupertype))
  }

}
