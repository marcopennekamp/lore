import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'lessons'

Deno.test(`${base}/hello-name`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/hello-name.lore`)
  assertListEquals(result, [
    'Hello, world.',
    'Hello, anonymous #5.',
    'Hello, marco.',
    'Hello, console.',
    'Hello, anonymous #42.'
  ])
})

Deno.test(`${base}/rna`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/rna.lore`)
  assertListEquals(result, ['u', 'a', 'c', 'g', 'ugcaccagaauu'])
})

Deno.test(`${base}/status`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/status.lore`)
  assertListEquals(result, ['Success!', 'Failure!', 'Success!', 'Failure!', 'Success!'])
})
