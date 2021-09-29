export const LoreMath = {
  floor(x: number): number {
    return Math.floor(x)
  },

  round(x: number): number {
    return Math.round(x)
  },

  ceil(x: number): number {
    return Math.ceil(x)
  },

  remainder(a: number, b: number): number {
    return a % b
  },

  pow(base: number, exponent: number): number {
    return Math.pow(base, exponent)
  },
}
