package lore.compiler.types

/**
  * A type path describes the path that must be taken to get to a specific subterm of a type. Type paths are currently
  * used to deduce open type arguments, but will eventually be used to transpile direct, optimized versions of dispatch
  * subtyping checks.
  *
  * Targets inside sum and intersection types currently cannot be represented as type paths because these types are
  * inherently unordered.
  */
case class TypePath(steps: Vector[TypePath.Step])

object TypePath {

  sealed trait Step

  /**
    * `Identity` signifies that the target has been reached. A valid path always ends in the identity step.
    */
  case object Identity extends Step

  /**
    * A part of a sum or intersection type identified via the type itself. We cannot use an index because sum and
    * intersection types are inherently unordered.
    *
    * Paths with `Part` steps cannot be transpiled.
    */
  case class Part(tpe: Type) extends Step

  case class TupleElement(index: Int) extends Step
  case object FunctionInput extends Step
  case object FunctionOutput extends Step
  case object ListElement extends Step
  case object MapKey extends Step
  case object MapValue extends Step
  case class ShapeProperty(name: String) extends Step
  case class TypeArgument(schema: DeclaredSchema, index: Int) extends Step

  /**
    * Get all type paths that lead to `target` given the `origin`.
    */
  def of(origin: Type, target: Type): Vector[TypePath] = {
    getSteps(origin, target).map(TypePath.apply)
  }

  /**
    * This function allows us to compute [[of]] without recreating countless instances of [[TypePath]].
    */
  private def getSteps(origin: Type, target: Type): Vector[Vector[Step]] = {
    if (origin == target) {
      return Vector(Vector(Identity))
    }

    def handleSubterm(subOrigin: Type, getStep: => Step) = {
      val possibilities = getSteps(subOrigin, target)
      if (possibilities.nonEmpty) {
        val step = getStep
        possibilities.map(steps => step +: steps)
      } else Vector.empty
    }

    def handleParts(parts: Set[Type]) = parts.toVector.flatMap(part => handleSubterm(part, Part(part)))
    origin match {
      case SumType(types) => handleParts(types)
      case IntersectionType(types) => handleParts(types)
      case TupleType(elements) => elements.zipWithIndex.flatMap {
        case (element, index) => handleSubterm(element, TupleElement(index))
      }
      case FunctionType(input, output) => handleSubterm(input, FunctionInput) ++ handleSubterm(output, FunctionOutput)
      case ListType(element) => handleSubterm(element, ListElement)
      case MapType(key, value) => handleSubterm(key, MapKey) ++ handleSubterm(value, MapValue)
      case ShapeType(properties) => properties.values.toVector.flatMap { property =>
        handleSubterm(property.tpe, ShapeProperty(property.name))
      }
      case dt: DeclaredType => dt.typeArguments.zipWithIndex.flatMap {
        case (typeArgument, index) => handleSubterm(typeArgument, TypeArgument(dt.schema, index))
      }
      case _ => Vector.empty
    }
  }

}
