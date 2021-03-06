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
export function benchmark(name: string, f: () => any, times: number): any {
  withSilentLogging(f)
  const before = performance.now()
  for (let i = 0; i < times; i += 1) {
    bucket = f()
  }
  const after = performance.now()

  const paddedName = name + ' '.repeat(Math.max(0, 60 - name.length))
  const ns = `${toNs(before, after, times)}ns`
  const paddedNs = ' '.repeat(Math.max(0, 16 - ns.length)) + ns
  console.log(`${paddedName}... ${paddedNs} [${bucket}]`)
  return bucket
}
