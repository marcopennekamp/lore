package lore.compiler.typing.unification

/*

import lore.compiler.types._
import lore.compiler.typing.InferenceBounds.BoundType
import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.utils.CollectionExtensions.VectorExtension

object Unification {

  /**
    * Unifies `t1` and `t2` such that `t1` is equal to `t2` in the given `boundTypes`, assigning inference variables
    * accordingly.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unifyEquals(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    EqualityUnification.unify(t1, t2, Vector(BoundType.Lower, BoundType.Upper), assignments)
  }

  /**
    * Unifies `t1` and `t2` such that `t1` is a subtype of `t2`, assigning inference variables accordingly.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unifySubtypes(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    SubtypingUnification.unify(t1, t2, isFit = false, assignments)
  }

  def unifySubtypes(ts1: Vector[Type], ts2: Vector[Type], assignments: Assignments): Option[Assignments] = {
    unifySubtypes(TupleType(ts1), TupleType(ts2), assignments)
  }

  /**
    * Unifies `t1` and `t2` such that `t1` is a subtype of `t2`, assigning inference variables accordingly. In contrast
    * to [[unifySubtypes]], `unifyFits` interprets inference variables as type parameters. Hence, inference variables
    * are assigned both bounds to immediately fix type parameter bounds.
    *
    * This approach is necessary to correctly assign type parameters whose instantiation will be `Nothing` or `Any`.
    * For example, if we have an argument of type `None` (i.e. an `Option[Nothing]`) and a parameter of type
    * `Option[iv1]`, we want `iv1` to be typed as `(Nothing, Nothing)`, not `(Nothing, Any)`, as this will make it
    * impossible to instantiate the correct candidate type.
    *
    * If the types cannot be unified, `None` is returned. Unification does not report errors on its own.
    */
  def unifyFits(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    SubtypingUnification.unify(t1, t2, isFit = true, assignments)
  }

  def unifyFits(ts1: Vector[Type], ts2: Vector[Type], assignments: Assignments): Option[Assignments] = {
    unifyFits(TupleType(ts1), TupleType(ts2), assignments)
  }

  /**
    * For the given `typeVariables` and their representation as inference variables via `typeVariableAssignments`, this
    * function unifies the current assignments of each inference variable with the bounds of its respective type
    * variable.
    */
  def unifyTypeVariableBounds(
    typeVariables: Vector[TypeVariable],
    typeVariableAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  ): Option[Assignments] = {
    typeVariables.foldSome(assignments) {
      case (assignments2, tv) => handleTypeVariableBounds(tv, typeVariableAssignments, assignments2)
    }
  }

  private def handleTypeVariableBounds(tv: TypeVariable, typeVariableAssignments: Map[TypeVariable, InferenceVariable], assignments: Assignments): Option[Assignments] = {
    val assignments2 = if (tv.lowerBound != BasicType.Nothing) {
      Unification.unifySubtypes(Type.substitute(tv.lowerBound, typeVariableAssignments), typeVariableAssignments(tv), assignments)
        .getOrElse(return None)
    } else assignments

    if (tv.upperBound != BasicType.Any) {
      Unification.unifySubtypes(typeVariableAssignments(tv), Type.substitute(tv.upperBound, typeVariableAssignments), assignments2)
    } else Some(assignments2)
  }

}

*/
