package lore.compiler.phases.typing

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error, Errors, Result}
import lore.compiler.phases.typing.Inference.{NonassignableType, variables}
import lore.compiler.phases.typing.InferenceAssignments.Assignments
import lore.compiler.semantics.Registry
import lore.compiler.types._

class Inference(judgments: Vector[TypingJudgment])(implicit registry: Registry) {

  def inferTypes(): Compilation[Assignments] = {
    var assignments: Assignments = Map.empty
    var workingSet = judgments.toSet

    // TODO: We probably need to solve this problem using a fixed-point approach, where the algorithm applies all
    //       resolvable judgments with each step until there are no more changes to the assignments.
    //       For example, let's say we have an equality a :=: b and b is already defined as b(lower: Nothing, upper: X).
    //       So we define a as the same. But then b is later defined as b(lower: Y, upper: Y) through a more complicated
    //       judgment. Then we have to redefine a as a(lower: Y, upper: Y).

    while (workingSet.nonEmpty) {
      // We have to pick judgments which we can resolve right away.
      // TODO: We certainly won't resolve cyclical relationships between inference variables this way. The question
      //       is whether that's a problem or not. :)
      val resolvable = workingSet.filter(isResolvable(assignments, _))
      if (resolvable.isEmpty) {
        throw CompilationException(s"The type inference working set $workingSet cannot be reduced any further. This is likely due to a flaw in the type inference algorithm.")
      }

      val compilation = resolvable.toVector.foldLeft(Compilation.succeed(assignments)) {
        case (compilation, typingJudgment) => compilation.flatMap { assignments => resolve(assignments, typingJudgment) }
      }

      compilation match {
        case Result(newAssignments, _) =>
          assignments = newAssignments
        case Errors(_, _) =>
          return compilation
      }

      workingSet = workingSet -- resolvable
    }

    // Once all inference variables have been instantiated, make another pass over all judgments to check equality
    // and subtyping constraints.
    judgments.map(check(assignments)).simultaneous.map(_ => assignments)
  }

  /**
    * A judgment is resolvable if all inference variables in a "source" position have been defined.
    */
  private def isResolvable(assignments: Assignments, judgment: TypingJudgment): Boolean = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      // The Equals judgment is resolvable if either t1 or t2 contain no undefined inference variables. Even if the
      // other type contains multiple inference variables, the judgment can be resolved by assigning multiple variables.
      Inference.variables(t1).forall(assignments.isDefined) || Inference.variables(t2).forall(assignments.isDefined)

    case TypingJudgment.Subtypes(_, t2, _) =>
      // TODO: In addition to the below: Is a subtyping relationship t1 :<: t2 resolvable if t1 contains no undefined
      //       inference variables but t2 does?
      Inference.variables(t2).forall(assignments.isDefined)

    case TypingJudgment.MemberAccess(_, source, _, _) =>
      Inference.variables(source).forall(assignments.isDefined)

    case TypingJudgment.LeastUpperBound(_, types, _) =>
      types.flatMap(Inference.variables).forall(assignments.isDefined)

    case TypingJudgment.Alternative(judgments, _) =>
      judgments.forall(isResolvable(assignments, _))
  }

  private def resolve(assignments: Assignments, judgment: TypingJudgment)(implicit registry: Registry): Compilation[Assignments] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      // If either type is not fully defined, we certainly want to apply the rule later once again to catch all
      // relationships. This plays into the idea that the constraints need to be applied with a fixed-point algorithm.
      val fullyDefined1 = Inference.variables(t1).forall(assignments.isDefined)
      val compilation1 = if (fullyDefined1) {
        assign(includeLowerBound = true, judgment)(assignments, assignments.instantiate(t1), t2)
      } else Compilation.succeed(assignments)

      compilation1.flatMap { assignments2 =>
        val fullyDefined2 = Inference.variables(t2).forall(assignments2.isDefined)
        if (fullyDefined2) {
          assign(includeLowerBound = true, judgment)(assignments2, assignments2.instantiate(t2), t1)
        } else Compilation.succeed(assignments2)
      }

    case TypingJudgment.Subtypes(t1, t2, _) =>
      if (Inference.variables(t2).forall(assignments.isDefined)) {
        // TODO: Is this order correct of assigning instantiated t2 to t1??
        assign(includeLowerBound = false, judgment)(assignments, assignments.instantiate(t2), t1)
      } else Compilation.succeed(assignments)

    case TypingJudgment.MemberAccess(target, source, name, _) =>
      assignments.instantiate(source).member(name)(judgment.position).flatMap { member =>
        // TODO: Shouldn't the lower bound be instantiated from the member type of the source's lower bound instantiation?
        //       If we have an inference variable a(lower: { member: A2 }, upper: { member: A1 }) with A2 < A1, we want
        //       a member access b <-- a.member to lead to an inference variable b(lower: A2, upper: A1).
        assignments.assignBounds(target, Some(member.tpe), member.tpe, judgment)
      }

    case TypingJudgment.LeastUpperBound(target, types, _) =>
      val lub = LeastUpperBound.leastUpperBound(types.map(assignments.instantiate))
      assignments.assignBounds(target, Some(lub), lub, judgment)

    case TypingJudgment.Alternative(judgments, _) =>
      ???
  }

  def assign(includeLowerBound: Boolean, context: TypingJudgment)(assignments: Assignments, source: Type, target: Type): Compilation[Assignments] = {
    val rec = assign(includeLowerBound, context) _

    // TODO: Don't we need to instantiate the types as best as we can before assigning them?

    // If the right-hand type contains no inference variables, there is no way we could assign anything, and thus the
    // assignment can be skipped. This check is currently important for correct compiler operation, as we only want to
    // raise an "unsupported substitution" error in cases where the right-hand type even contains type variables.
    if (variables(target).isEmpty) {
      return Compilation.succeed(assignments)
    }

    if (variables(source).nonEmpty) {
      throw CompilationException(s"The source $source should have been assigned to target $target, but the source still contains inference variables.")
    }

    // TODO: Can we even live with unsupported substitutions here or do we have to bite the bullet? Sum and
    //       intersection types need to be type checked...
    def unsupportedSubstitution: Nothing = {
      throw CompilationException(s"Intersection and sum type type inference assignments are not yet supported." +
        s" Given types: $source and $target.")
    }

    def nonassignableType: Compilation[Assignments] = Compilation.fail(NonassignableType(source, target, context))

    (source, target) match {
      case (_, iv2: InferenceVariable) =>
        val lowerBound = if (includeLowerBound) Some(source) else None
        assignments.assignBounds(iv2, lowerBound, source, context)

      case (tv1: TypeVariable, tv2: TypeVariable) =>
        // TODO: Do we need to assign lower and upper bounds of type variables for inference????
        ???

      case (p1: ProductType, p2: ProductType) =>
        if (p1.elements.size == p2.elements.size) {
          p1.elements.zip(p2.elements).foldLeft(Compilation.succeed(assignments)) {
            case (compilation, (e1, e2)) => compilation.flatMap { assignments => rec(assignments, e1, e2) }
          }
        } else nonassignableType

      case (f1: FunctionType, f2: FunctionType) => rec(assignments, f1.input, f2.input).flatMap(rec(_, f1.output, f2.output))

      case (l1: ListType, l2: ListType) => rec(assignments, l1.element, l2.element)

      case (m1: MapType, m2: MapType) => rec(assignments, m1.key, m2.key).flatMap(rec(_, m1.value, m2.value))

      case (s1: ShapeType, s2: ShapeType) =>
        s2.correlate(s1).foldLeft(Compilation.succeed(assignments)) {
          case (compilation, (p2, Some(p1))) => compilation.flatMap { assignments => rec(assignments, p1.tpe, p2.tpe) }
          case (_, (_, None)) => nonassignableType
        }
      case (s1: StructType, s2: ShapeType) => rec(assignments, s1.asShapeType, s2)

      // Allocating types to intersection types and sum types is quite complex, since the allocation mechanism
      // suddenly comes upon more than one possible allocation. Take, for example, a sum type A | B, to which we
      // try to assign a type C. Should A or B become C? Surely not both A and B can be C (unless the sum type
      // is trivial). And even if we have a structurally similar type C | D, should A = C and B = D or A = D and
      // B = C? There are multiple possibilities.
      case (_: IntersectionType, _) => unsupportedSubstitution
      case (_, _: IntersectionType) => unsupportedSubstitution
      case (_: SumType, _) => unsupportedSubstitution
      case (_, _: SumType) => unsupportedSubstitution

      // In all other cases, there is no need to assign anything.
      // TODO: Declared types will be able to contain inference variables when they become polymorphic.
      case _ => Compilation.succeed(assignments)
    }
  }

  /**
    * Checks the given typing judgment as instantiated by the variables in the given assignments. This ensures that
    * type equality and subtyping relationships actually hold.
    */
  private def check(assignments: Assignments)(judgment: TypingJudgment): Verification = judgment match {
    case TypingJudgment.Equals(t1, t2, position) =>
      if (assignments.instantiate(t1) != assignments.instantiate(t2)) {
        Compilation.fail() // TODO: Types not equal.
      } else Verification.succeed

    case TypingJudgment.Subtypes(t1, t2, position) =>
      if (!(assignments.instantiate(t1) <= assignments.instantiate(t2))) {
        Compilation.fail() // TODO: Types aren't subtypes.
      } else Verification.succeed

    case _ => Verification.succeed
  }

}

object Inference {

  // TODO: Test and improve error messages.

  case class InvalidLowerBound(inferenceVariable: InferenceVariable, actual: Type, expected: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"Type error: $actual must be a supertype of $expected."
  }

  case class InvalidUpperBound(inferenceVariable: InferenceVariable, actual: Type, expected: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"Type error: $actual must be a subtype of $expected."
  }

  case class NonassignableType(source: Type, target: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"bla blub" // TODO
  }

  def variables(tpe: Type): Set[InferenceVariable] = tpe match {
    case iv: InferenceVariable => Set(iv)
    case tv: TypeVariable => variables(tv.lowerBound) ++ variables(tv.upperBound)
    case SumType(types) => types.flatMap(variables)
    case IntersectionType(types) => types.flatMap(variables)
    case ProductType(elements) => elements.flatMap(variables).toSet
    case FunctionType(input, output) => variables(input) ++ variables(output)
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case ShapeType(properties) => properties.values.map(_.tpe).flatMap(variables).toSet
    case _: NamedType => Set.empty // TODO: Update when struct/trait types can have type parameters.
  }

  /**
    * Whether the given type contains no inference variables at all, which means that it can bypass type inference.
    */
  def isFullyInferred(tpe: Type): Boolean = variables(tpe).isEmpty

}
