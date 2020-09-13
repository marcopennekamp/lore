package lore.compiler.types

import java.io.ByteArrayOutputStream

import lore.compiler.core.CompilationException

/**
  * Encodes types in a binary representation.
  *
  * The binary representation is defined as follows:
  *   - Types are represented in Polish notation, which makes it possible to generate a unique identifier
  *     without using parentheses.
  *   - The first byte of a type's representation, the tag, determines the kind of the type and, possibly,
  *     its number of operands. Types are divided into variable-size types, fixed-size types and basic types:
  *       - Variable size: Sum, Intersection, Product, Named
  *         - Named has, for now, always zero operands, as we have not introduced parametric structs/traits yet.
  *         - Also note that Named excludes type variables!
  *       - Basic type: Any, Nothing, Real, Int, Boolean, String
  *       - Fixed size: List, Map, Component, Variable
  *     The first three bits determine the kind of the type:
  *       - 000: Sum
  *       - 001: Intersection
  *       - 010: Product
  *       - 011: Named
  *       - 100: basic type
  *       - 101: fixed-size type
  *     The last five bits are determined as follows:
  *       - Sum/Intersection/Product/Named: the number of operands (0 to 31)
  *       - basic type:
  *         - 00000: Any
  *         - 00001: Nothing
  *         - 00010: Real
  *         - 00011: Int
  *         - 00100: Boolean
  *         - 00101: String
  *       - fixed-size type:
  *         - 00000: List
  *         - 00001: Map
  *         - 00010: Component
  *         - 00100: Variable without bounds (lower bound: Nothing, upper bound: Any)
  *         - 00101: Variable with custom lower bound (upper bound: Any)
  *         - 00110: Variable with custom upper bound (lower bound: Nothing)
  *         - 00111: Variable with custom bounds
  *   - Following the first byte are any operands. Concretely:
  *     - Sum/Intersection/Product:
  *       - Any child types according to the encoded number of operands.
  *     - Named:
  *       - The name of the type, encoded as a UTF-8 string with a length.
  *       - Any child types according to the encoded number of operands.
  *     - Basic types: No operands.
  *     - List: A single element type.
  *     - Map: A key type and a value type.
  *     - Component: A single underlying type.
  *     - Variable:
  *       - The name of the variable, encoded as a UTF-8 string with a length.
  *       - The lower and/or upper bound based on the specific kind, as outlined above.
  */
object TypeEncoder {
  private object Kind {
    val sum: Byte = 0
    val intersection: Byte = 1
    val product: Byte = 2
    val named: Byte = 3
    val basic: Byte = 4
    val fixedSize: Byte = 5
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
    val list: Byte = Tag(Kind.fixedSize, 0)
    val map: Byte = Tag(Kind.fixedSize, 1)
    val component: Byte = Tag(Kind.fixedSize, 2)
    val variableNothingAny: Byte = Tag(Kind.fixedSize, 4)
    val variableAny: Byte = Tag(Kind.fixedSize, 5)
    val variableNothing: Byte = Tag(Kind.fixedSize, 6)
    val variable: Byte = Tag(Kind.fixedSize, 7)
  }

  def encode(tpe: Type): Array[Byte] = {
    implicit val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    writeType(tpe)
    stream.toByteArray
  }

  private def writeType(t: Type)(implicit stream: ByteArrayOutputStream): Unit = {
    // TODO: Order intersection and sum parts to disambiguate sets.
    val (tag, name, children) = t match {
      case SumType(types) => (Tag.variableSize(Kind.sum, types.size), None, types)
      case IntersectionType(types) => (Tag.variableSize(Kind.intersection, types.size), None, types)
      case ProductType(elements) => (Tag.variableSize(Kind.product, elements.size), None, elements)
      case ListType(element) => (Tag.list, None, List(element))
      case MapType(key, value) => (Tag.map, None, List(key, value))
      case ComponentType(underlying) => (Tag.component, None, List(underlying))
      case tv: TypeVariable =>
        val (tag, bounds) = (tv.lowerBound, tv.upperBound) match {
          case (BasicType.Nothing, BasicType.Any) => (Tag.variableNothingAny, Nil)
          case (_, BasicType.Any) => (Tag.variableAny, List(tv.lowerBound))
          case (BasicType.Nothing, _) => (Tag.variableNothing, List(tv.upperBound))
          case _ => (Tag.variable, List(tv.lowerBound, tv.upperBound))
        }
        (tag, Some(tv.name), bounds)
      case t: BasicType => (Tag.basic(t), None, Nil)
      case t: NamedType => (Tag(Kind.named), Some(t.name), Nil)
    }

    stream.write(tag)
    name.foreach(writeString)
    children.foreach(writeType)
  }

  /**
    * Writes a string to the stream as UTF-8, encoding its length using one or two additional bytes (in big-endian).
    */
  private def writeString(string: String)(implicit stream: ByteArrayOutputStream): Unit = {
    val bytes = string.getBytes("UTF-8")
    // If the length is less than 128, we have one length byte with a leftmost 0. If the length is less than 32768,
    // we have two length bytes with the leftmost bit of the first byte always a 1. This ensures that the first
    // bit of the string's length correctly encodes whether we have extended length or not.
    bytes.length match {
      case l if l < 128 => stream.write(l)
      case l if l < 32768 => stream.write(0x80 | ((l & 0x7F00) >>> 8)); stream.write(l & 0xFF)
      case _ => throw CompilationException("Names with lengths of more than ")
    }
    stream.write(bytes)
  }
}
