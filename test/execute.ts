import { JsonConversion } from '../runtime/src/lore/runtime/utils/JsonConversion.ts'

const programFile = Deno.args[0]
const { test } = await import(`./${programFile}`)
const result = test()
console.log(JsonConversion.toJson(result))
