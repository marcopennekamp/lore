package lore.compiler.types

import lore.compiler.core.CompilationException

/**
  * Encodes types in a binary representation.
  *
  * The binary representation is defined as follows:
  *   - Types are represented in Polish notation, which makes it possible to generate a unique identifier
  *     without using parentheses.
  *   - The first byte of a type's representation, the tag, determines the kind of the type and, possibly,
  *     its number of operands. Types are divided into variable-size types, fixed-size types and basic types:
  *       - Variable size: Sum, Intersection, Tuple, Named
  *         - Named has, for now, always zero operands, as we have not introduced parametric structs/traits yet.
  *         - Also note that Named excludes type variables!
  *       - Basic type: Any, Nothing, Real, Int, Boolean, String
  *       - Fixed size: Function, List, Map, Variable, Atom
  *     The first three bits determine the kind of the type:
  *       - 000: Sum
  *       - 001: Intersection
  *       - 010: Tuple
  *       - 011: Named
  *       - 100: Shape
  *       - 101: basic type
  *       - 110: fixed-size type
  *     The last five bits are determined as follows:
  *       - Sum/Intersection/Tuple/Named: the number of operands (0 to 31)
  *       - Shape: the number of properties (0 to 31)
  *       - basic type:
  *         - 00000: Any
  *         - 00001: Nothing
  *         - 00010: Real
  *         - 00011: Int
  *         - 00100: Boolean
  *         - 00101: String
  *       - fixed-size type:
  *         - 00000: Function
  *         - 00001: List
  *         - 00010: Map
  *         - 00100: Variable without bounds (lower bound: Nothing, upper bound: Any)
  *         - 00101: Variable with custom lower bound (upper bound: Any)
  *         - 00110: Variable with custom upper bound (lower bound: Nothing)
  *         - 00111: Variable with custom bounds
  *         - 01000: Atom
  *   - Following the first byte are any operands. Concretely:
  *     - Sum/Intersection/Tuple:
  *       - Any child types according to the encoded number of operands.
  *     - Named:
  *       - The name of the type, encoded as a UTF-8 string with a length.
  *       - Any child types according to the encoded number of operands.
  *     - Shape:
  *       - For each property contained in the shape, ordered alphabetically:
  *         - The property's name, encoded as a UTF-8 string with a length.
  *         - The property's type.
  *     - Basic types: No operands.
  *     - Function: An input type and an output type.
  *     - List: A single element type.
  *     - Map: A key type and a value type.
  *     - Variable:
  *       - The name of the variable, encoded as a UTF-8 string with a length.
  *       - The lower and/or upper bound based on the specific kind, as outlined above.
  *     - Atom: The name of the atom, encoded as a UTF-8 string with a length.
  */
object TypeEncoder {

  private object Kind {
    val sum: Byte = 0
    val intersection: Byte = 1
    val tuple: Byte = 2
    val named: Byte = 3
    val shape: Byte = 4
    val basic: Byte = 5
    val fixedSize: Byte = 6
  }

  private object Tag {
    def apply(kind: Byte): Byte = apply(kind, 0)
    def apply(kind: Byte, args: Byte): Byte = ((kind << 5) | (args & 0x1F)).toByte

    def variableSize(kind: Byte, length: Int): Byte = {
      if (length > 31) throw CompilationException("A variable-size type with more than 31 operands cannot be encoded.")
      Tag(kind, length.toByte)
    }

    def basic(tpe: BasicType): Byte = {
      val args: Byte = tpe match {
        case BasicType.Any => 0
        case BasicType.Nothing => 1
        case BasicType.Real => 2
        case BasicType.Int => 3
        case BasicType.Boolean => 4
        case BasicType.String => 5
      }
      Tag(Kind.basic, args)
    }

    // Fixed-size types.
    val function: Byte = Tag(Kind.fixedSize, 0)
    val list: Byte = Tag(Kind.fixedSize, 1)
    val map: Byte = Tag(Kind.fixedSize, 2)
    val variableNothingAny: Byte = Tag(Kind.fixedSize, 4)
    val variableAny: Byte = Tag(Kind.fixedSize, 5)
    val variableNothing: Byte = Tag(Kind.fixedSize, 6)
    val variable: Byte = Tag(Kind.fixedSize, 7)
    val atom: Byte = Tag(Kind.fixedSize, 8)
  }

  def encode(tpe: Type): Vector[Byte] = writeType(tpe)

  private def writeType(t: Type): Vector[Byte] = t match {
    // TODO: Order intersection and sum parts to disambiguate sets.
    case SumType(types) => Tag.variableSize(Kind.sum, types.size) +: types.toVector.flatMap(writeType)
    case IntersectionType(types) => Tag.variableSize(Kind.intersection, types.size) +: types.toVector.flatMap(writeType)
    case TupleType(elements) => Tag.variableSize(Kind.tuple, elements.size) +: elements.flatMap(writeType)
    case FunctionType(input, output) => (Tag.function +: writeType(input)) ++ writeType(output)
    case ListType(element) => Tag.list +: writeType(element)
    case MapType(key, value) => (Tag.map +: writeType(key)) ++ writeType(value)
    case ShapeType(properties) =>
      val propertyBytes = properties.values.toVector.sortBy(_.name).flatMap(property => writeString(property.name) ++ writeType(property.tpe))
      Tag.variableSize(Kind.shape, properties.size) +: propertyBytes
    case AtomType(name) => Tag.atom +: writeString(name)
    case tv: TypeVariable =>
      val (tag, bounds) = (tv.lowerBound, tv.upperBound) match {
        case (BasicType.Nothing, BasicType.Any) => (Tag.variableNothingAny, Vector.empty)
        case (_, BasicType.Any) => (Tag.variableAny, writeType(tv.lowerBound))
        case (BasicType.Nothing, _) => (Tag.variableNothing, writeType(tv.upperBound))
        case _ => (Tag.variable, writeType(tv.lowerBound) ++ writeType(tv.upperBound))
      }
      (tag +: writeString(tv.name)) ++ bounds
    case t: BasicType => Vector(Tag.basic(t))
    case t: NamedType => Tag(Kind.named) +: writeString(t.name)
  }

  /**
    * Writes a string to the result as UTF-8, encoding its length using one or two additional bytes (big-endian).
    */
  private def writeString(string: String): Vector[Byte] = {
    val bytes = string.getBytes("UTF-8")
    // If the length is less than 128, we have one length byte with a leftmost 0. If the length is less than 32768,
    // we have two length bytes with the leftmost bit of the first byte always a 1. This ensures that the first
    // bit of the string's length correctly encodes whether we have extended length or not.
    bytes.length match {
      case l if l < 128 => l.toByte +: bytes.toVector
      case l if l < 32768 => (0x80 | ((l & 0x7F00) >>> 8)).toByte +: ((l & 0xFF).toByte +: bytes.toVector)
      case _ => throw CompilationException("Names with lengths of more than 32767 UTF-8 bytes cannot be encoded.")
    }
  }

}
