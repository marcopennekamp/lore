import { Lists, ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { TupleValue } from '../../../runtime/src/lore/runtime/tuples.ts'
import { Types } from '../../../runtime/src/lore/runtime/types/types.ts'
import { assertIsTuple, assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/schemas'

Deno.test(`${base}/alias`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/alias.lore`)
  assertListEquals(result, [
    'hey int 5',
    'hey int 2',
    '',
    'hey int 1, hey int 2, hey int 3',
    '2',
    '0.8',
    '4',
    '1',
  ])
})

Deno.test(`${base}/array`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/array.lore`)
  assertListEquals(result, [2, 3, 4])
})

Deno.test(`${base}/constructor`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/constructor.lore`)
  assertListEquals(result, ['cereal bowl', 'cereal cup', 'blood bowl', 'blood cup', 'pudding bowl', 'pudding cup'])
})

Deno.test(`${base}/extract`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/extract.lore`)
  assertListEquals(result, ['5', '[1, 2, 3]', 'I am a crate and my name is Toby!'])
})

Deno.test(`${base}/fluxify`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/fluxify.lore`)
  assertListEquals(result, [13, 8, 19])
})

Deno.test(`${base}/goods`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/goods.lore`)
  assertListEquals(result, [48.5, 17.2, 1.6])
})

Deno.test(`${base}/open-properties`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/open-properties.lore`)
  assertListEquals(result, ['thing with content', 'thing with content', 'thing with hammer'])
})

Deno.test(`${base}/option`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/option.lore`)
  assertListEquals(result, [
    'None',
    'Some(Hello, world!)',
    'Some([1, 2, 3])',
    '[It\'s just a bird., My gosh, the pollution up there!, Oh shit, it\'s a dragon!]',
    'They\'re everywhere!',
  ])
})

Deno.test(`${base}/type-filter`, async () => {
  const result: TupleValue = await LoreTest.run(`${base}/type-filter.lore`)
  assertIsTuple(result, [Lists.type(Types.number), Lists.type(Types.string)])
  assertListEquals(result.elements[0], [12, 5, 37])
  assertListEquals(result.elements[1], ['hello world', 'cool world'])
})
