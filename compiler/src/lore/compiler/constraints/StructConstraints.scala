package lore.compiler.constraints

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Reporter, StructFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.types.{DeclaredType, FunctionType, IntersectionType, ListType, MapType, ShapeType, SumType, TupleType, Type, TypeVariable}
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.OptionVectorExtension

object StructConstraints {

  /**
    * Verifies:
    *   1. Properties must be unique.
    *   2. The properties of the struct's inherited shape type must all be defined.
    *   3. Co-/contra-/invariant type parameters must be used in appropriate positions in property types.
    *   4. Open type parameters must be covariant, uniquely deducible, and used in immutable properties.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyPropertiesUnique(definition)
    verifyInheritedShapeProperties(definition)
    verifyVariancePositions(definition)
    verifyOpenTypeParameters(definition)
  }

  /**
    * Verifies that the given struct's properties are unique.
    */
  private def verifyPropertiesUnique(definition: StructDefinition)(implicit reporter: Reporter): Unit = {
    definition.properties.verifyUnique(_.name, property => StructFeedback.DuplicateProperty(definition, property))
  }

  /**
    * Verifies that the given struct's inherited shape type is properly implemented.
    */
  private def verifyInheritedShapeProperties(definition: StructDefinition)(implicit reporter: Reporter): Unit = {
    definition.schema.inheritedShapeType.properties.values.toVector.foreach { shapeProperty =>
      definition.propertyMap.get(shapeProperty.name) match {
        case Some(structProperty) =>
          if (structProperty.tpe </= shapeProperty.tpe) {
            reporter.error(StructFeedback.Shape.InvalidPropertyType(definition, structProperty, shapeProperty))
          }
        case None => reporter.error(StructFeedback.Shape.MissingProperty(definition, shapeProperty))
      }
    }
  }

  /**
    * Verifies that co-/contra-/invariant type parameters are used in appropriate positions in property types.
    */
  private def verifyVariancePositions(definition: StructDefinition)(implicit reporter: Reporter): Unit = {
    definition.properties.foreach { property =>
      val origin = if (property.isMutable) Variance.Invariant else Variance.Covariant
      VarianceConstraints.verifyVariance(property.tpe, origin, property.position)
    }
  }

  /**
    * Verifies that open type parameters are covariant, uniquely deducible, and used in immutable properties.
    */
  private def verifyOpenTypeParameters(definition: StructDefinition)(implicit reporter: Reporter): Unit = {
    definition.schema.openParameters.foreach { typeParameter =>
      if (typeParameter.variance != Variance.Covariant) {
        reporter.error(StructFeedback.OpenTypeParameter.CovarianceRequired(typeParameter, definition.position))
      }

      definition.schema.derivingProperties.get(typeParameter) match {
        case None => reporter.error(StructFeedback.OpenTypeParameter.NotUniquelyDeducible(typeParameter, definition.position))
        case Some(property) =>
          if (property.isMutable) {
            reporter.error(StructFeedback.OpenTypeParameter.MutableProperty(typeParameter, property))
          }
          if (!isUniquelyDeducible(typeParameter, property.tpe)) {
            reporter.error(StructFeedback.OpenTypeParameter.NotUniquelyDeducible(typeParameter, property.position))
          }
      }
    }
  }

  /**
    * Whether the given type parameter is contained in `tpe` exactly once and in a way that allows a
    * [[lore.compiler.types.TypeVariableAllocation]] to assign a type to the type parameter deterministically.
    */
  private def isUniquelyDeducible(typeParameter: TypeVariable, tpe: Type): Boolean = {
    // Counts the occurrences of `typeParameter` in the given subterm. If a sum or intersection type contains an
    // occurrence, the result is instead `None`, because an open type argument cannot be deduced from a sum or
    // intersection type. `None` is interpreted as "not uniquely deducible".
    def multiCount(types: Vector[Type]): Option[Int] = types.map(count).sequence.map(_.sum)
    def count: Type => Option[Int] = {
      case tv: TypeVariable if typeParameter == tv => Some(1)
      case SumType(types) => multiCount(types.toVector).filter(_ == 0)
      case IntersectionType(types) => multiCount(types.toVector).filter(_ == 0)
      case TupleType(elements) => multiCount(elements)
      case FunctionType(input, output) => multiCount(Vector(input, output))
      case ListType(element) => count(element)
      case MapType(key, value) => multiCount(Vector(key, value))
      case ShapeType(properties) => multiCount(properties.values.map(_.tpe).toVector)
      case dt: DeclaredType => multiCount(dt.typeArguments)
      case _ => Some(0)
    }
    count(tpe).contains(1)
  }

}
