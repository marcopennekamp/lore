const programFile = Deno.args[0]
const { test } = await import(`./${programFile}`)
const result = test()
console.log(JSON.stringify(result))
