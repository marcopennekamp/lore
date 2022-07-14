package lore.compiler.constraints

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Reporter, StructFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.definitions.BindingDefinitionKind
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionVectorExtension

object StructConstraints {

  /**
    * Verifies:
    *   1. Properties must be unique.
    *   2. The properties of the struct's inherited shape type must all be defined.
    *   3. Expression constraints for all default value expressions.
    *   4. Co-/contra-/invariant type parameters must be used in appropriate positions in property types.
    *   5. Open type parameters must be covariant, uniquely deducible, and used in immutable properties.
    *
    * Additionally, if the struct is an object:
    *   1. All properties must have default values.
    *   2. None of the properties may share a name with the companion module's members.
    */
  def verify(schema: StructSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyPropertiesUnique(schema)
    verifyInheritedShapeProperties(schema)
    verifyDefaultValueExpressionConstraints(schema)
    verifyVariancePositions(schema)
    verifyOpenTypeParameters(schema)

    if (schema.isObject) {
      verifyObjectDefaults(schema)
      verifyObjectCompanionNames(schema)
    }
  }

  /**
    * Verifies that the given struct's properties are unique.
    */
  private def verifyPropertiesUnique(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.properties.verifyUnique(_.name, property => StructFeedback.DuplicateProperty(schema, property))
  }

  /**
    * Verifies that the given struct's inherited shape type is properly implemented.
    */
  private def verifyInheritedShapeProperties(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.inheritedShapeType.properties.values.toVector.foreach { shapeProperty =>
      schema.propertyMap.get(shapeProperty.name) match {
        case Some(structProperty) =>
          if (structProperty.tpe </= shapeProperty.tpe) {
            reporter.error(StructFeedback.Shape.InvalidPropertyType(schema, structProperty, shapeProperty))
          }
        case None => reporter.error(StructFeedback.Shape.MissingProperty(schema, shapeProperty))
      }
    }
  }

  /**
    * Verifies expression constraints for all default values.
    */
  private def verifyDefaultValueExpressionConstraints(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.properties.foreach { property =>
      property.defaultValueNode.foreach(ExpressionConstraints.verify)
    }
  }

  /**
    * Verifies that co-/contra-/invariant type parameters are used in appropriate positions in property types.
    */
  private def verifyVariancePositions(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.properties.foreach { property =>
      val origin = if (property.isMutable) Variance.Invariant else Variance.Covariant
      VarianceConstraints.verifyVariance(property.tpe, origin, property.position)
    }
  }

  /**
    * Verifies that open type parameters are covariant, uniquely deducible, and used in immutable properties.
    */
  private def verifyOpenTypeParameters(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.openParameters.foreach { typeParameter =>
      if (typeParameter.variance != Variance.Covariant) {
        reporter.error(StructFeedback.OpenTypeParameter.CovarianceRequired(typeParameter, schema.position))
      }

      if (typeParameter.lowerBound != BasicType.Nothing) {
        reporter.error(StructFeedback.OpenTypeParameter.IllegalLowerBound(typeParameter, schema.position))
      }

      schema.openParameterDerivations.get(typeParameter) match {
        case None => reporter.error(StructFeedback.OpenTypeParameter.NotUniquelyDeducible(typeParameter, schema.position))
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
    * Whether the given type parameter is contained in `tpe` exactly once, and in a way that allows a run-time type
    * variable allocation to assign a type to the type parameter deterministically.
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

  /**
    * Verifies that the properties of the given object all have default values.
    */
  private def verifyObjectDefaults(schema: StructSchema)(implicit reporter: Reporter): Unit = {
    schema.properties.filterNot(_.hasDefault).foreach {
      property => reporter.error(StructFeedback.Object.MissingDefault(schema, property))
    }
  }

  /**
    * Verifies that none of the properties of the given object share a name with its companion module's members.
    *
    * This constraint allows us to be a bit laxer when handling struct objects in scopes. Normally, a struct object's
    * property would have to shadow a companion module's member. But this comes with two problems. First, accessing the
    * member would at most be possible via imports. And second, the shadowing would make name resolution much more
    * complex, especially when resolving absolute paths.
    */
  private def verifyObjectCompanionNames(schema: StructSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    registry.getModule(schema.name).foreach { companionModule =>
      schema.properties.foreach { property =>
        companionModule.terms.get(property.name).foreach { term =>
          reporter.error(StructFeedback.Object.MemberNameTaken(schema, property.name, term.position))
        }
      }
    }
  }

}
