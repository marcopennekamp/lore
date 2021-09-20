import { List, ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { Types } from '../../runtime/src/lore/runtime/types/types.ts'
import { assertIsTuple, assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('schemas/alias', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/alias.lore')
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

Deno.test('schemas/array', async () => {
  const result: ListValue<number> = await LoreTest.run('schemas/array.lore')
  assertListEquals(result, [2, 3, 4])
})

Deno.test('schemas/constructor', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/constructor.lore')
  assertListEquals(result, ['cereal bowl', 'cereal cup', 'blood bowl', 'blood cup', 'pudding bowl', 'pudding cup'])
})

Deno.test('schemas/extract', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/extract.lore')
  assertListEquals(result, ['5', '[1, 2, 3]', 'I am a crate and my name is Toby!'])
})

Deno.test('schemas/fluxify', async () => {
  const result: ListValue<number> = await LoreTest.run('schemas/fluxify.lore')
  assertListEquals(result, [13, 8, 19])
})

Deno.test('schemas/goods', async () => {
  const result: ListValue<number> = await LoreTest.run('schemas/goods.lore')
  assertListEquals(result, [48.5, 17.2, 1.6])
})

Deno.test('schemas/open-properties', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/open-properties.lore')
  assertListEquals(result, ['thing with content', 'thing with content', 'thing with hammer'])
})

Deno.test('schemas/option', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/option.lore')
  assertListEquals(result, [
    'None',
    'Some(Hello, world!)',
    'Some([1, 2, 3])',
    '[It\'s just a bird., My gosh, the pollution up there!, Oh shit, it\'s a dragon!]',
    'They\'re everywhere!',
  ])
})

Deno.test('schemas/type-filter', async () => {
  const result: TupleValue = await LoreTest.run('schemas/type-filter.lore')
  assertIsTuple(result, [List.type(Types.int), List.type(Types.string)])
  assertListEquals(result.elements[0], [12, 5, 37])
  assertListEquals(result.elements[1], ['hello world', 'cool world'])
})
