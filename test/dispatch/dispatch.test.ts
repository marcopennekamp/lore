import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('dispatch/abstract-partial-specialization', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/abstract-partial-specialization')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['A1,B', 'A1,B', 'A2,B', 'A2,B'])
})

Deno.test('dispatch/abstract-sum', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/abstract-sum')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, [3, 4, 4, 5, 5, 1, 2])
})

Deno.test('dispatch/abstract-sum-intersection', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/abstract-sum-intersection')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['(A|B)&T', '(A|B)&T', 'C&T'])
})

Deno.test('dispatch/hello-name', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/hello-name')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['Hello, world.', 'Hello, anonymous #5.', 'Hello, marco.', 'Hello, console.', 'Hello, anonymous #100.'])
})

Deno.test('dispatch/intersection', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/intersection')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['X&W,Y&L2', 'X,Y', 'Y,Z&L1', 'Z,W&L1', 'Y&L,X&Y', 'Y&L1,X&Y', 'Y&W&L1,X&Y', 'Z,W&L1', 'T1,T2'])
})

Deno.test('dispatch/parametric', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/parametric')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, [1, 3, 2, 1, 3, 2, 4])
})
