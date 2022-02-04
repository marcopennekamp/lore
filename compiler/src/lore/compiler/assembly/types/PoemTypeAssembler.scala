package lore.compiler.assembly.types

import lore.compiler.poem._
import lore.compiler.types._

object PoemTypeAssembler {

  def generate(tpe: Type): PoemType = tpe match {
    case tv: TypeVariable => PoemTypeVariable(tv.index)
    case bt: BasicType => PoemBasicType(bt)
    case SumType(parts) => PoemXaryType(Kind.Sum, parts.toVector.map(generate))
    case IntersectionType(parts) => PoemXaryType(Kind.Intersection, parts.toVector.map(generate))
    case TupleType(elements) => PoemXaryType(Kind.Tuple, elements.map(generate))
    case FunctionType(input, output) => PoemXaryType(Kind.Function, Vector(generate(input), generate(output)))
    case ListType(element) => PoemXaryType(Kind.List, Vector(generate(element)))
    case MapType(key, value) => PoemXaryType(Kind.Map, Vector(generate(key), generate(value)))
    case ShapeType(properties) => PoemShapeType(properties.map { case (name, property) => name -> generate(property.tpe) })
    case SymbolType(name) => PoemSymbolType(name)
    case dt: DeclaredType => PoemNamedType(dt.schema, dt.typeArguments.map(generate))
  }

}
