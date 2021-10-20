import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../../runtime/src/lore/runtime/structs.ts'
import { assertListEquals, assertStructHasValues } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/dispatch'

Deno.test(`${base}/fixed`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/fixed.lore`)
  assertListEquals(result, ['ABC', 'EABD', 'AB', 'ABD', 'A', 'A'])
})

Deno.test(`${base}/identity`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/identity.lore`)
  assertListEquals(result, [0, 1.2, 'Hello', true])
})

Deno.test(`${base}/intersection`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/intersection.lore`)
  assertListEquals(result, ['X&W,Y&L2', 'X,Y', 'Y,Z&L1', 'Z,W&L1', 'Y&L,X&Y', 'Y&L1,X&Y', 'Y&W&L1,X&Y', 'Z,W&L1', 'T1,T2'])
})

Deno.test(`${base}/movable`, async () => {
  const zombie: StructValue = await LoreTest.run(`${base}/movable.lore`)
  assertStructHasValues(zombie, 'Zombie', { name: 'Fred' })
  assertStructHasValues((zombie as any).position, 'Position', { x: 1.7, y: 2.5, z: 0.5 })
})

Deno.test(`${base}/parametric`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/parametric.lore`)
  assertListEquals(result, [1, 3, 2, 2, 3, 2, 4])
})

Deno.test(`${base}/tuple`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/tuple.lore`)
  assertListEquals(result, [
    'Consume string and int!',
    'Consume int and any!',
    'Consume any and any!',
    'Consume boolean and string!',
    'Consume any and any!',
  ])
})
