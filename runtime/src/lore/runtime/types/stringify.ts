import { FunctionType } from '../functions.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { StructType } from '../structs.ts'
import { DeclaredType } from './declared-types.ts'
import { Kind } from './kinds.ts'
import { PropertyTypes } from './property-types.ts'
import { TypeVariable } from './type-variables.ts'
import { Type, XaryType } from './types.ts'

export function stringify(type: Type): string {
  switch (type.kind) {
    case Kind.TypeVariable: return (<TypeVariable> type).fullName

    case Kind.Any: return 'Any'
    case Kind.Nothing: return 'Nothing'
    case Kind.Real: return 'Real'
    case Kind.Int: return 'Int'
    case Kind.Boolean: return 'Boolean'
    case Kind.String: return 'String'

    case Kind.Trait: return stringifyDeclaredType(<DeclaredType> type)
    case Kind.Struct: {
      const struct = <StructType> type
      const declaredTypeString = stringifyDeclaredType(struct)
      if (struct.propertyTypes) {
        return `${declaredTypeString}(${stringifyPropertyTypes(struct.propertyTypes).join(', ')})`
      }
      return declaredTypeString
    }

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

function stringifyDeclaredType(dt: DeclaredType): string {
  const name = dt.schema.name
  if (dt.typeArguments) {
    return `${name}[${dt.typeArguments.map(stringify).join(', ')}]`
  }
  return name
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
