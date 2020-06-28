let basket = ''

function hello(arg) {
  if (typeof arg === 'string') {
    //console.log(`Hello, ${arg}.`)
    basket = `Hello, ${arg}.`
  } else if (typeof arg === 'number') {
    //console.log(`Hello, anonymous #${arg}.`)
    basket = `Hello, anonymous #${arg}.`
  }
}

function test() {
  let names = ['world', 5, 'marco']
  names.push('console')
  names.push(100)
  names.forEach((name) => {
    hello(name)
  })
}

function testPerformanceInner(times) {
  const now = performance.now()
  let i = 0
  while (i < times) {
    test()
    i += 1
  }
  const now2 = performance.now()
  return now2 - now
}

function testPerformance(times) {
  // Pre-run for the JIT.
  const time1 = testPerformanceInner(times)
  console.log(`Pre-Running test() ${times} times took ${time1}ms.`)

  // Then run ten times and see how performance evolves.
  let i = 0
  while (i < 10) {
    const time = testPerformanceInner(times)
    console.log(`Running test() ${times} times took ${time}ms.`)
    i += 1
  }
}

testPerformance(100000);
