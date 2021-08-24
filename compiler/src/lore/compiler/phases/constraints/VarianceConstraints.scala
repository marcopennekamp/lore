package lore.compiler.phases.constraints

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types.{DeclaredType, FunctionType, IntersectionType, ListType, MapType, ShapeType, SumType, TupleType, Type, TypeVariable}

object VarianceConstraints {

  case class InvalidVariance(typeVariable: TypeVariable, origin: Variance, override val position: Position) extends Feedback.Error(position) {
    override val message: String = s"The ${typeVariable.variance.humanReadable} type variable $typeVariable is in an" +
      s" illegal ${origin.humanReadable} position."
  }

  /**
    * Verifies that the given type contains only type variables with the given origin variance or invariance. Certain
    * type positions such as function input types and type arguments of contravariant type parameters may flip some
    * origin variances.
    */
  def verifyVariance(tpe: Type, origin: Variance, position: Position)(implicit reporter: Reporter): Unit = {
    def rec = t => verifyVariance(t, origin, position)
    def recFlipped = t => verifyVariance(t, Variance.flip(origin), position)

    tpe match {
      case tv: TypeVariable if tv.variance != origin && tv.variance != Variance.Invariant => reporter.error(InvalidVariance(tv, origin, position))
      case SumType(types) => types.foreach(rec)
      case IntersectionType(types) => types.foreach(rec)
      case TupleType(elements) => elements.foreach(rec)
      case FunctionType(input, output) =>
        recFlipped(input)
        rec(output)
      case ListType(element) => rec(element)
      case MapType(key, value) =>
        rec(key)
        rec(value)
      case ShapeType(properties) => properties.values.map(_.tpe).foreach(rec)
      case dt: DeclaredType =>
        dt.assignments.foreach {
          case (typeParameter, typeArgument) => typeParameter.variance match {
            case Variance.Covariant => rec(typeArgument)
            case Variance.Contravariant => recFlipped(typeArgument)
            case Variance.Invariant => verifyVariance(typeArgument, Variance.Invariant, position)
          }
        }
      case _ =>
    }
  }

}
