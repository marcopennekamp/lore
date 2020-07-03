import { testPerformance } from '../lore-program.js';
import Lore from './src/lore/runtime/Lore.ts';
import { product, int, real } from './src/lore/runtime/types/types.ts';

testPerformance(500);


function toNs(before, after) {
  return Math.ceil((after - before) * 1000000);
}


const before = performance.now();
const types = [];
for (let i = 0; i < 10000; i += 1) {
  types.push(product([]));
}
const after = performance.now();
console.log('type creation took ' + toNs(before, after) + 'ns, so ' + Math.ceil(toNs(before, after) / 10000) + 'ns per iteration');
