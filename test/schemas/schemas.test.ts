import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('schemas/array', async () => {
  const result: ListValue<number> = await LoreTest.run('schemas/array.lore')
  assertListEquals(result, [2, 3, 4])
})

Deno.test('schemas/option', async () => {
  const result: ListValue<string> = await LoreTest.run('schemas/option.lore')
  assertListEquals(result, ['None', 'Some(Hello, world!)', 'Some([1, 2, 3])', 'Oh shit, it\'s a dragon!'])
})
