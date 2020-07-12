import { test } from '../lore-program.js';

function toMs(before, after) {
  return Math.round((after - before) * 1000) / 1000;
}

function withSilentLogging(f) {
  const log = console.log;
  console.log = function() { }
  f();
  console.log = log;
}

// Note that the time taken by test() most likely represents the time it takes the Javascript interpreter to execute
// the program. The function is also run once before performance is measured so that it can be prepared, parsed, and
// compiled to bytecode if it hasn't been done yet. Interpreted performance is likely about 10-20 times slower than
// JIT performance, so keep that in mind if you're developing "high performance" code.

// If we don't use console.log before the performance timings, the results are actually slightly slower than expected.
// Hence, we also remove this variable from the test environment.
withSilentLogging(test);
console.log('Running test...');

const before = performance.now();
test();
const after = performance.now();
console.log('Running test (most likely interpreted) took about ' + toMs(before, after) + 'ms.');
