package lore.types

import scala.collection.mutable

trait Subtyping {
  /**
    * We define subtyping like this so that we can test all rules when the pattern matches multiple rules.
    * The alternative would be using a case expression, which would greedily hone in on the first pattern
    * match, unless we use guards, which would make the code quite messy.
    */
  val subtypingRules: Seq[PartialFunction[(Type, Type), Boolean]] = Seq(
    // A declared type d1 is a subtype of d2 if d1 and d2 are equal or any of d1's hierarchical supertypes equal to d2.
    { case (d1: DeclaredType, d2: DeclaredType) =>  d1 == d2 || isSubtype(d1.supertype.getOrElse(AnyType), d2) },

    // A component type p1 is a subtype of p2 if p1's underlying type is a subtype of p2's underlying type.
    { case (p1: ComponentType, p2: ComponentType) => isSubtype(p1.underlying, p2.underlying) },
    // An entity type e1 is a subtype of a component type p2 if e1 has a component type p1 that is a subtype of p2.
    { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => isSubtype(p1, p2)) },

    // Whether t1 is a subtype of a type variable t2 essentially comes down to type bounds. This isn't sufficient
    // to check all properties that pertain to parameterized multi-functions, but covers the "subtyping" component.
    // TODO: We have a slight problem: When a class type is invariant, we don't want C[String] to be a subtype of
    //       C[Any], UNLESS we really have a C[String] <: C[X <: Any]. Maybe to check types against type variables,
    //       we have to move away from the notion of subtyping and to a concept of "instance equality", which
    //       calculates equality on the basis of whether one type can be equal to the other type if all type
    //       variables are instanced correctly.
    { case (v1: TypeVariable, v2: TypeVariable) => isSubtype(v1.bound, v2.bound) },
    { case (v1: TypeVariable, t2) => isSubtype(v1.bound, t2) },
    { case (t1, v2: TypeVariable) => isSubtype(t1, v2.bound) },

    // An intersection type i1 is the subtype of an intersection type i2, if all types in i2 are subsumed by i1.
    { case (i1: IntersectionType, i2: IntersectionType) => i2.types.forall(ic2 => i1.isAnyComponentSubtypeOf(ic2)) },
    // An intersection type i1 is the subtype of another type t2 if one component of i1 is a subtype of t2.
    { case (i1: IntersectionType, t2) => i1.isAnyComponentSubtypeOf(t2) },
    // A non-intersection type t1 is the subtype of an intersection type i2, if t1 is the subtype of all types in i2.
    { case (t1, i2: IntersectionType) => i2.types.forall(ic2 => isSubtype(t1, ic2)) },

    // A sum type s1 is the subtype of a sum type s2, if all types in s1 are also (possibly supertyped) in s2.
    { case (s1: SumType, s2: SumType) => s1.types.forall(sc1 => s2.types.exists(sc2 => isSubtype(sc1, sc2))) },
    // A type t1 is the subtype of a sum type s2, if t1 is a subtype of any of the types in s2.
    // TODO: This should be defined over subsets of s2: For example, A | B <= A | B | C, but this does not hold with the current code.
    { case (t1, s2: SumType) => s2.types.exists(sc2 => isSubtype(t1, sc2)) },
    // A sum type s1 is the subtype of a non-sum type t2, if all individual types in s1 are subtypes of t2.
    // More formally: A <= C and B <= C implies A | B <= C
    { case (s1: SumType, t2) => s1.types.forall(sc1 => isSubtype(sc1, t2)) },

    // A product type tt1 is the subtype of a product type tt2, if both types have the same number of component types
    // and each component type of tt1 is a subtype of the component type in tt2 that is at the same position.
    {
      case (p1: ProductType, p2: ProductType) =>
        p1.components.size == p2.components.size && p1.components.zip(p2.components).forall {
          case (c1, c2) => isSubtype(c1, c2)
        }
    },

    // Lists are covariant.
    { case (l1: ListType, l2: ListType) => isSubtype(l1.element, l2.element) },

    // Handle basic types. Int is a subtype of Real.
    { case (a: BasicType, b: BasicType) => a eq b },
    { case (BasicType.Int, BasicType.Real) => true },

    // The Any type is supertype of all types.
    { case (_, AnyType) => true },

    // The Nothing type is subtype of all types.
    { case (NothingType, _) => true },
  )

  // TODO: For later: If we want to support sum types and intersection types, they (kind of) require us to
  //       branch with the substitution, as outlined below.
  class SubstitutionFamily

  class Substitutions {
    private val assignments = mutable.HashMap[TypeVariable, List[Type]]()
    private var areAssignmentsCompatible = false

    /**
      * Assigns the given type to the type variable.
      */
    def assign(tv: TypeVariable, tpe: Type): Unit = {
      val types = assignments.getOrElse(tv, Nil)
      assignments.put(tv, tpe +: types)
    }

    /**
      * In the given type, replaces all instances of type variables registered here with their assigned representative.
      * You must have checked for assignment compatibility first (by reading isConsistent for the first time).
      */
    def substituteInto(in: Type): Type = {
      if (!areAssignmentsCompatible) {
        throw new RuntimeException("Assignments are incompatible or haven't been checked yet!")
      }

      in match {
        case tv: TypeVariable => assignments.get(tv) match {
          case None => tv
          case Some(types) => types.head
        }
        case ProductType(components) => ProductType(components.map(substituteInto))
        case SumType(types) => SumType(types.map(substituteInto))
        case IntersectionType(types) => IntersectionType(types.map(substituteInto))
        case ListType(element) => ListType(substituteInto(element))
        case MapType(key, value) => MapType(substituteInto(key), substituteInto(value))
        case t => t
      }
    }

    /**
      * Whether these substitutions are consistent. We essentially check two properties:
      *   1. All types assigned to the same variable are compatible (equal) to each other.
      *   2. Type assignments are consistent with type bounds.
      */
    lazy val isConsistent: Boolean = {
      // Check the "compatible assignments" property.
      areAssignmentsCompatible = assignments.forall { case (_, assignments) =>
        assignments.sliding(2).forall {
          case List(left, right) =>
            // Since equality is transitive, we don't have to compare all types to each other.
            left == right
          case List(_) =>
            // List(_).sliding(2) will return List(_), so we have to manually evaluate to true for the special
            // case of one-element lists.
            true
        }
      }

      if (areAssignmentsCompatible) {
        // Check the "type bounds" property.
        assignments.forall { case (variable, representative :: _) =>
          val actualBound = substituteInto(variable.bound)
          isSubtype(representative, actualBound)
        }
      } else false
    }
  }

  def possibleSubstitutions(t1: Type, t2: Type)(implicit substitutions: Substitutions): Unit = {
    def unsupportedSubstitution: Nothing = {
      throw new RuntimeException("Intersection and sum type type variable substitutions are not yet supported.")
    }
    (t1, t2) match {
      case (_, tv2: TypeVariable) => substitutions.assign(tv2, t1)
      // TODO: Substitute type variables in class types.
      case (l1: ListType, l2: ListType) => possibleSubstitutions(l1.element, l2.element)
      case (m1: MapType, m2: MapType) =>
        // TODO: Is this correct?
        possibleSubstitutions(m1.key, m2.key)
        possibleSubstitutions(m1.value, m2.value)
      case (p1: ProductType, p2: ProductType) =>
        if (p1.components.size == p2.components.size) {
          p1.components.zip(p2.components).foreach { case (c1, c2) => possibleSubstitutions(c1, c2) }
        }

      // Substituting types into intersection types and sum types is quite complex, since the substitution mechanism
      // suddenly has more than one option where to substitute into. Take, for example, a sum type A | B, in which we
      // try to substitute a type C. Should A or B become C? Surely not both A and B can be C. And even if we have a
      // structurally similar type C | D, should A = C and B = D or A = D and B = C? There are multiple possibilities.
      // We don't have to assign variables perfectly to check for subtyping, but there are some possibilities which
      // will lead to the answer "not a subtype", while others will lead to the answer "is a subtype". Hence, we have
      // to consider all these possibilities, which makes the algorithm vastly more complex.
      case (_: IntersectionType, _) => unsupportedSubstitution
      case (_, _: IntersectionType) => unsupportedSubstitution
      case (_: SumType, _) => unsupportedSubstitution
      case (_, _: SumType) => unsupportedSubstitution

      // In all other cases, there is no need to substitute anything. Note that component types can't contain
      // type variables, currently, as they expect a class type.
      case _ =>
    }
  }

  /**
    * Whether t1 is a subtype of t2.
    *
    * To check subtyping with parametric types, we have to ensure two properties:
    *   1. t1 is a subtype of t2 when parametric types are viewed as their type bounds.
    *   2. Assignments from t1 to type variables of t2 are consistent.
    */
  def isSubtype(t1: Type, t2: Type): Boolean = {
    // If t2 is parametric, we have to check that assignments to its type variables are consistent. This effectively
    // ensures that all instances of the type variable are assigned .
    if (t2.isParametric) {
      implicit val substitutions: Substitutions = new Substitutions
      possibleSubstitutions(t1, t2)
      if (!substitutions.isConsistent) {
        println("Inconsistent substitutions.")
        return false
      }
    }

    // TODO: We might need to use a more complex theorem solver with proper typing rules instead of such an ad-hoc/greedy algorithm.
    //       This is actually working so far, though, and we need it to be fast because of the runtime reality.
    // t1 is a subtype of t2 if any of the rules are true.
    for (rule <- subtypingRules) {
      if (rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2))) return true
    }
    false
  }

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
}

object Subtyping extends Subtyping
