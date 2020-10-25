export function toNs(before: number, after: number, times: number = 1): number {
  return Math.round((after - before) * 1000000 / times)
}

export function toµs(before: number, after: number, times: number = 1): number {
  return Math.round((after - before) * 1000 / times)
}

export function toMs(before: number, after: number, times: number = 1): number {
  return toµs(before, after, times) / 1000
}

export function withSilentLogging(f: () => any): void {
  const log = console.log
  console.log = function() { }
  f()
  console.log = log
}

let bucket: any = undefined
export function benchmark(name: string, f: () => any, times: number): void {
  withSilentLogging(f)
  const before = performance.now()
  for (let i = 0; i < times; i += 1) {
    bucket = f()
  }
  const after = performance.now()
  console.log(`Benchmark '${name}' took about ${toNs(before, after, times)}ns per operation. It had the following result:`)
  console.log(bucket)
}
