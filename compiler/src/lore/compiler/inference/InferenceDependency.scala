package lore.compiler.inference

import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.SetExtension

/**
  * A `source`/`target` pair where the `target` inference variable explicitly depends on the `source` type or
  * inference variable.
  */
case class InferenceDependency(source: Type, target: InferenceVariable)

object InferenceDependency {

  def dependenciesOf(judgment: TypingJudgment): Set[InferenceDependency] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) => biDependenciesOf(t1, t2)
    case TypingJudgment.Subtypes(t1, t2, _) => biDependenciesOf(t1, t2)
    case TypingJudgment.Assign(target, source, _) => dependenciesOf(source, target)
    case TypingJudgment.Fits(t1, t2, _) => dependenciesOf(t1, t2)
    case TypingJudgment.LeastUpperBound(target, types, _) => biDependenciesOf(Vector(target), types)
    case TypingJudgment.MemberAccess(target, source, _, _) => biDependenciesOf(target, source)
    case TypingJudgment.ElementType(target, collection, _) => dependenciesOf(collection, target)
    case TypingJudgment.MultiFunctionCall(target, _, arguments, _) => dependenciesOf(arguments, Vector(target))
    case TypingJudgment.MultiFunctionValue(_, _, _) => Set.empty // TODO: Change this once MultiFunctionValues depend on the target.
    case hint@TypingJudgment.MultiFunctionHint(_, _, _) => dependenciesOf(Vector(hint.dependencyVariable), hint.argumentTypes)
  }

  private def dependenciesOf(sources: Set[Type], targets: Set[InferenceVariable]): Set[InferenceDependency] = {
    targets.flatMap(target => sources.map(InferenceDependency(_, target)))
  }

  /**
    * Find dependencies from all variables in `source` to all variables in `target`. If `source` contains no variables,
    * find all dependencies from `source` as a constant type to all variables in `target`.
    */
  private def dependenciesOf(source: Type, target: Type): Set[InferenceDependency] = {
    dependenciesOf(
      Inference.variables(source).asInstanceOf[Set[Type]].ifEmptySingle(source),
      Inference.variables(target)
    )
  }

  /**
    * Find dependencies from all variables in `sources` to all variables in `targets`. If `sources` contain no
    * variables, find all dependencies from `sources` as constant types to all variables in `targets`.
    */
  private def dependenciesOf(sources: Vector[Type], targets: Vector[Type]): Set[InferenceDependency] = {
    val sourceVariables = sources.flatMap(Inference.variables).toSet
    val targetVariables = targets.flatMap(Inference.variables).toSet
    dependenciesOf(sourceVariables.asInstanceOf[Set[Type]].ifEmpty(sources.toSet), targetVariables)
  }

  private def biDependenciesOf(t1: Type, t2: Type): Set[InferenceDependency] = {
    dependenciesOf(t1, t2) ++ dependenciesOf(t2, t1)
  }

  private def biDependenciesOf(types1: Vector[Type], types2: Vector[Type]): Set[InferenceDependency] = {
    dependenciesOf(types1, types2) ++ dependenciesOf(types2, types1)
  }

}
