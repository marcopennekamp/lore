import { assertEquals, assertNotEquals } from 'https://deno.land/std/testing/asserts.ts'
import { stringHash, orderedHash, unorderedHash } from '../../../../src/lore/runtime/utils/hash.ts'

const obj1 = { hash: stringHash("daiwbf34832im") }
const obj2 = { hash: stringHash("lawji23rwhigs") }
const obj3 = { hash: stringHash("r92n3kfuwn2sö") }
const obj4 = { hash: stringHash("8ujrl2äärkana") }
const obj5 = { hash: stringHash("093jklkngalwl") }

Deno.test("utils/hash: ordered hashes are correctly computed", () => {
  const hash = orderedHash

  // 0 Elements.
  assertEquals(hash([]), hash([]))
  assertNotEquals(hash([]), hash([obj1]))

  // 1 Element.
  assertEquals(hash([obj1]), hash([obj1]))
  assertNotEquals(hash([obj1]), hash([obj2]))

  // 2 Elements.
  assertEquals(hash([obj1, obj2]), hash([obj1, obj2]))
  assertNotEquals(hash([obj1, obj2]), hash([obj2, obj1]))
  assertNotEquals(hash([obj1, obj2]), hash([obj2]))

  // 3 Elements.
  assertEquals(hash([obj1, obj2, obj5]), hash([obj1, obj2, obj5]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj1, obj5, obj2]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj2, obj1, obj5]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj2, obj5, obj1]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj5, obj1, obj2]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj5, obj2, obj1]))

  assertNotEquals(hash([obj1, obj5, obj2]), hash([obj5, obj2, obj1]))
  assertNotEquals(hash([obj5, obj2, obj1]), hash([obj2, obj5, obj1]))

  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj1, obj2]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj1, obj5]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj2, obj5]))

  // 5 Elements.
  assertEquals(hash([obj1, obj2, obj3, obj4, obj5]), hash([obj1, obj2, obj3, obj4, obj5]))
  assertNotEquals(hash([obj3, obj1, obj2, obj5, obj4]), hash([obj2, obj4, obj5, obj3, obj1]))

  // Non-unique elements.
  assertEquals(hash([obj1, obj2, obj2]), hash([obj1, obj2, obj2]))
  assertNotEquals(hash([obj1, obj2, obj2]), hash([obj2, obj2, obj1]))
  assertNotEquals(hash([obj1, obj2]), hash([obj1, obj2, obj2]))
})

Deno.test("utils/hash: unordered hashes are correctly computed", () => {
  const hash = unorderedHash

  // 0 Elements.
  assertEquals(hash([]), hash([]))
  assertNotEquals(hash([]), hash([obj1]))

  // 1 Element.
  assertEquals(hash([obj1]), hash([obj1]))
  assertNotEquals(hash([obj1]), hash([obj2]))

  // 2 Elements.
  assertEquals(hash([obj1, obj2]), hash([obj1, obj2]))
  assertEquals(hash([obj1, obj2]), hash([obj2, obj1]))
  assertNotEquals(hash([obj1, obj2]), hash([obj2]))

  // 3 Elements.
  assertEquals(hash([obj1, obj2, obj5]), hash([obj1, obj2, obj5]))
  assertEquals(hash([obj1, obj2, obj5]), hash([obj1, obj5, obj2]))
  assertEquals(hash([obj1, obj2, obj5]), hash([obj2, obj1, obj5]))
  assertEquals(hash([obj1, obj2, obj5]), hash([obj2, obj5, obj1]))
  assertEquals(hash([obj1, obj2, obj5]), hash([obj5, obj1, obj2]))
  assertEquals(hash([obj1, obj2, obj5]), hash([obj5, obj2, obj1]))

  assertEquals(hash([obj1, obj5, obj2]), hash([obj5, obj2, obj1]))
  assertEquals(hash([obj5, obj2, obj1]), hash([obj2, obj5, obj1]))

  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj1, obj2]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj1, obj5]))
  assertNotEquals(hash([obj1, obj2, obj5]), hash([obj2, obj5]))

  // 5 Elements.
  assertEquals(hash([obj1, obj2, obj3, obj4, obj5]), hash([obj1, obj2, obj3, obj4, obj5]))
  assertEquals(hash([obj3, obj1, obj2, obj5, obj4]), hash([obj2, obj4, obj5, obj3, obj1]))

  // Non-unique elements.
  assertEquals(hash([obj1, obj2, obj2]), hash([obj1, obj2, obj2]))
  assertEquals(hash([obj1, obj2, obj2]), hash([obj2, obj2, obj1]))
  assertNotEquals(hash([obj1, obj2]), hash([obj1, obj2, obj2]))
})
