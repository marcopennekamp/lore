import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ObjectValue } from '../runtime/src/lore/runtime/values/object.ts'
import { Kind } from '../runtime/src/lore/runtime/types/kinds.ts'

export function assertIsStruct(value: ObjectValue, fullName: string) {
  assertEquals(value.lore$type.kind, Kind.Struct)
  assertEquals(value.lore$type.schema.name, fullName)
}
