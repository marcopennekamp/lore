import { FunctionType } from '../functions.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { StructType } from '../structs.ts'
import { TraitType } from '../traits.ts'
import { Kind } from './kinds.ts'
import { TypeVariable } from './type-variables.ts'
import { PropertyTypes, Type, XaryType } from './types.ts'

export function stringify(type: Type): string {
  switch (type.kind) {
    case Kind.TypeVariable: return (<TypeVariable> type).name

    case Kind.Any: return 'Any'
    case Kind.Nothing: return 'Nothing'
    case Kind.Real: return 'Real'
    case Kind.Int: return 'Int'
    case Kind.Boolean: return 'Boolean'
    case Kind.String: return 'String'

    case Kind.Struct: {
      const struct = <StructType> type
      if (struct.propertyTypes) {
        return `${struct.schema.name}(${stringifyPropertyTypes(struct.propertyTypes).join(', ')})`
      }
      return struct.schema.name
    }
    case Kind.Trait: return (<TraitType> type).schema.name

    case Kind.Sum: return stringifyXary(<XaryType> type, " | ")
    case Kind.Intersection: return stringifyXary(<XaryType> type, " & ")
    case Kind.Tuple: return stringifyXary(<XaryType> type, ", ")

    case Kind.Function: {
      const func = <FunctionType> type
      return `${stringify(func.input)} => ${stringify(func.output)}`
    }

    case Kind.List: return `[${stringify((<ListType> type).element)}]`
    case Kind.Map: {
      const map = <MapType> type
      return `${stringify(map.key)} -> ${stringify(map.value)}`
    }
    case Kind.Shape: {
      const shape = <ShapeType> type
      return '{ ' + stringifyPropertyTypes(shape.propertyTypes).join(', ') + ' }'
    }
  }

  return ''
}

function stringifyXary(type: XaryType, separator: string): string {
  return `(${type.types.map(stringify).join(separator)})`
}

function stringifyPropertyTypes(propertyTypes: PropertyTypes | undefined): Array<string> {
  if (!propertyTypes) {
    return []
  }
  return Object.entries(propertyTypes).map(([name, propertyType]) => `${name}: ${stringify(propertyType)}`)
}
