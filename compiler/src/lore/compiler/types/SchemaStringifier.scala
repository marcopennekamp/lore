package lore.compiler.types

object SchemaStringifier {

  /**
    * Creates a pretty string representation of the type schema.
    */
  def toString(schema: TypeSchema, verbose: Boolean = false): String = toStringWithPrecedence(schema, verbose, TypePrecedence.Parenthesized)

  private sealed abstract class TypePrecedence(protected val value: Int) {
    def <(precedence: TypePrecedence): Boolean = this.value <= precedence.value
  }

  private object TypePrecedence {
    case object Parenthesized extends TypePrecedence(0)
    case object Sum extends TypePrecedence(1)
    case object Intersection extends TypePrecedence(2)
    case object Function extends TypePrecedence(3)
    case object Map extends TypePrecedence(4)
  }

  private def toStringWithPrecedence(schema: TypeSchema, verbose: Boolean = false, parentPrecedence: TypePrecedence): String = {
    val infix = stringifyInfixOperator(parentPrecedence, toStringWithPrecedence(_, verbose, _)) _

    schema match {
      case SumType(types) => infix(" | ", TypePrecedence.Sum, types.toVector)
      case IntersectionType(types) => infix(" & ", TypePrecedence.Intersection, types.toVector)
      case TupleType(elements) =>
        if (elements.isEmpty) "Unit"
        else s"(${elements.map(toString(_, verbose)).mkString(", ")})"
      case FunctionType(input, output) => infix(" => ", TypePrecedence.Function, Vector(input, output))
      case ListType(element) => s"[${toString(element, verbose)}]"
      case MapType(key, value) => infix(" -> ", TypePrecedence.Map, Vector(key, value))
      case ShapeType(properties) =>
        val propertyRepresentations = properties.values.map { property =>
          s"${property.name}: ${toString(property.tpe, verbose)}"
        }
        s"%{ ${propertyRepresentations.mkString(", ")} }"
      case SymbolType(name) => s"#$name"
      case dt: DeclaredType =>
        val typeArguments = if (dt.typeArguments.nonEmpty) s"[${dt.typeArguments.map(toString(_, verbose)).mkString(", ")}]" else ""
        if (verbose) {
          val kind = dt match {
            case _: StructType => "struct"
            case _: TraitType => "trait"
          }
          val extended = if (dt.schema.supertypes.nonEmpty) s" extends ${dt.schema.supertypes.map(toString(_, verbose)).mkString(", ")}" else ""
          s"$kind ${dt.name}$typeArguments$extended"
        } else s"${dt.name.simpleName}$typeArguments"
      case t: NamedSchema => t.name.simpleName
      case _ => schema.toString
    }
  }

  private def stringifyInfixOperator(
    parentPrecedence: TypePrecedence,
    stringify: (Type, TypePrecedence) => String,
  )(operator: String, operatorPrecedence: TypePrecedence, operands: Vector[Type]): String = {
    val repr = operands.map(stringify(_, operatorPrecedence)).mkString(operator)
    if (operatorPrecedence < parentPrecedence) s"($repr)" else repr
  }

}
