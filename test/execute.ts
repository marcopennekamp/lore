/**
 * Filters properties from JSON.stringify that would lead to circular references.
 */
function replacer(key: string, value: any) {
  if (key === 'representative') return undefined
  return value
}

const programFile = Deno.args[0]
const { test } = await import(`./${programFile}`)
const result = test()
console.log(JSON.stringify(result, replacer))
