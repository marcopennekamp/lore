import { test } from '../lore-program.js';

function toMs(before, after) {
  return Math.round((after - before) * 1000) / 1000;
}

const before = performance.now();
test();
const after = performance.now();
console.log('Running test took about ' + toMs(before, after) + 'ms.');
