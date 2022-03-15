package lore.compiler.assembly.types

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.poem._
import lore.compiler.types._

object TypeAssembler {

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

  def generateParameter(tv: TypeVariable): PoemTypeParameter = {
    PoemTypeParameter(
      tv.name.toString,
      TypeAssembler.generate(tv.lowerBound),
      TypeAssembler.generate(tv.upperBound),
      tv.variance,
    )
  }

  /**
    * Generates a `TypeConst` or `TypeArg` instruction to load the given type into a register. `TypeConst` will
    * substitute type variables in `tpe` if it contains any. Using `TypeArg` is a simple optimization that isn't
    * necessary for correctness.
    */
  def generateTypeConst(tpe: Type)(implicit registerProvider: RegisterProvider): AsmChunk = {
    val regResult = registerProvider.fresh()
    val instruction = tpe match {
      case tv: TypeVariable => PoemInstruction.TypeArg(regResult, tv.index)
      case _ => PoemInstruction.TypeConst(regResult, TypeAssembler.generate(tpe))
    }
    AsmChunk(regResult, instruction)
  }

}
