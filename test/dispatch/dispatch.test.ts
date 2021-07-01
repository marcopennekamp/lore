import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { assertListEquals, assertSumTypeParts } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('dispatch/abstract-intersection', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/abstract-intersection.lore')
  assertListEquals(result, ['A', 'C', 'B', 'C', 'C', 'B', 'C', 'B', 'C', 'B', 'X1|X2', 'X1|X2', 'X3'])
})

Deno.test('dispatch/abstract-partial-specialization', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/abstract-partial-specialization.lore')
  assertListEquals(result, ['A1,B', 'A1,B', 'A2,B', 'A2,B'])
})

Deno.test('dispatch/abstract-sum', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/abstract-sum.lore')
  assertListEquals(result, [3, 4, 4, 5, 5, 1, 2])
})

Deno.test('dispatch/fixed', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/fixed.lore')
  assertListEquals(result, ['ABC', 'EABD', 'AB', 'ABD', 'A', 'A'])
})

Deno.test('dispatch/hello-name', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/hello-name.lore')
  assertListEquals(result, [
    'Hello, world.',
    'Hello, anonymous #5.',
    'Hello, marco.',
    'Hello, console.',
    'Hello, anonymous #100.'
  ])
})

Deno.test('dispatch/intersection', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/intersection.lore')
  assertListEquals(result, ['X&W,Y&L2', 'X,Y', 'Y,Z&L1', 'Z,W&L1', 'Y&L,X&Y', 'Y&L1,X&Y', 'Y&W&L1,X&Y', 'Z,W&L1', 'T1,T2'])
})

Deno.test('dispatch/parametric', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/parametric.lore')
  assertListEquals(result, [1, 3, 2, 1, 3, 2, 4])
})

Deno.test('dispatch/savable', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/savable.lore')
  assertListEquals(result, ['Character', 'Position', 1.5, 2.7, 'ExternalPhysics', 42])

  // Because the list only contains String and Real values, its type should be [String | Real].
  assertSumTypeParts(result.lore$type.element, ({ kind }) => kind === Kind.Real || kind === Kind.String)
})

Deno.test('dispatch/tuple', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/tuple.lore')
  assertListEquals(result, [
    'Consume string and int!',
    'Consume int and any!',
    'Consume any and any!',
    'Consume boolean and string!',
    'Consume any and any!',
  ])
})
