import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { assertListEquals, assertSumTypeParts } from '../assertions.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('dispatch/abstract-partial-specialization', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/abstract-partial-specialization')
  assertListEquals(result, ['A1,B', 'A1,B', 'A2,B', 'A2,B'])
})

Deno.test('dispatch/abstract-sum', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/abstract-sum')
  assertListEquals(result, [3, 4, 4, 5, 5, 1, 2])
})

Deno.test('dispatch/abstract-sum-intersection', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/abstract-sum-intersection')
  assertListEquals(result, ['(A|B)&T', '(A|B)&T', 'C&T'])
})

Deno.test('dispatch/hello-name', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/hello-name')
  assertListEquals(result, [
    'Hello, world.',
    'Hello, anonymous #5.',
    'Hello, marco.',
    'Hello, console.',
    'Hello, anonymous #100.'
  ])
})

Deno.test('dispatch/intersection', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/intersection')
  assertListEquals(result, ['X&W,Y&L2', 'X,Y', 'Y,Z&L1', 'Z,W&L1', 'Y&L,X&Y', 'Y&L1,X&Y', 'Y&W&L1,X&Y', 'Z,W&L1', 'T1,T2'])
})

Deno.test('dispatch/parametric', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/parametric')
  assertListEquals(result, [1, 3, 2, 1, 3, 2, 4])
})

Deno.test('dispatch/savable', async () => {
  const result: ListValue<number> = await LoreTest.run('dispatch/savable')
  assertListEquals(result, ['Character', 'Position', 1.5, 2.7, 'ExternalPhysics', 42])

  // Because the list only contains String and Real values, its type should be [String | Real].
  assertSumTypeParts(result.lore$type.element, ({ kind }) => kind === Kind.Real || kind === Kind.String)
})

Deno.test('dispatch/tuple', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/tuple')
  assertListEquals(result, [
    'Consume string and int!',
    'Consume int and any!',
    'Consume any and any!',
    'Consume boolean and string!',
    'Consume any and any!',
  ])
})
