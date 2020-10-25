import { parse } from 'https://deno.land/std/flags/mod.ts';
import { test } from '../lore-program.js'
import { benchmark } from './benchmarks/benchmark.ts'

// Note that the time taken by test() most likely represents the time it takes the Javascript interpreter to execute
// the program. The function is also run once before performance is measured so that it can be prepared, parsed, and
// compiled to bytecode if it hasn't been done yet. Interpreted performance is likely about 10-20 times slower than
// JIT performance, so keep that in mind if you're developing "high performance" code.

const args = parse(Deno.args)
const repetitions = args.n ?? 1

benchmark('test', test, repetitions)
