package lore.compiler.types

/**
  * A type path describes the path that must be taken to get to a specific subterm of a type. Type paths are used to
  * deduce open type arguments.
  *
  * Targets inside sum and intersection types cannot be reached via type paths because these types are inherently
  * unordered.
  */
case class TypePath(steps: Vector[TypePath.Step])

object TypePath {

  sealed trait Step

  case class TupleElement(index: Int) extends Step
  case object FunctionInput extends Step
  case object FunctionOutput extends Step
  case object ListElement extends Step
  case object MapKey extends Step
  case object MapValue extends Step
  case class ShapeProperty(name: String) extends Step
  case class TypeArgument(schema: DeclaredSchema, index: Int) extends Step

  /**
    * Get all type paths that lead to `target` from `origin`.
    */
  def of(origin: Type, target: Type): Vector[TypePath] = {
    getSteps(origin, target).map(TypePath.apply)
  }

  /**
    * This function allows us to compute [[of]] without recreating countless instances of [[TypePath]].
    */
  private def getSteps(origin: Type, target: Type): Vector[Vector[Step]] = {
    if (origin == target) {
      return Vector(Vector.empty)
    }

    // Each possible branch in `origin` is tried and followed until `target` is found. If `target` cannot be found, the
    // list of paths for the branch is empty and the branch is ignored.
    def handleBranch(subOrigin: Type, getStep: => Step) = {
      val possibilities = getSteps(subOrigin, target)
      if (possibilities.nonEmpty) {
        val step = getStep
        possibilities.map(steps => step +: steps)
      } else Vector.empty
    }

    origin match {
      case TupleType(elements) => elements.zipWithIndex.flatMap {
        case (element, index) => handleBranch(element, TupleElement(index))
      }

      case FunctionType(input, output) => handleBranch(input, FunctionInput) ++ handleBranch(output, FunctionOutput)
      case ListType(element) => handleBranch(element, ListElement)
      case MapType(key, value) => handleBranch(key, MapKey) ++ handleBranch(value, MapValue)

      case ShapeType(properties) => properties.values.toVector.flatMap { property =>
        handleBranch(property.tpe, ShapeProperty(property.name))
      }

      case dt: DeclaredType => dt.typeArguments.zipWithIndex.flatMap {
        case (typeArgument, index) => handleBranch(typeArgument, TypeArgument(dt.schema, index))
      }

      case _ => Vector.empty
    }
  }

}
