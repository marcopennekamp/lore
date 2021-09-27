export const Numbers = {
  parse(string: string): number {
    return parseInt(string)
  },

  isNan(number: number): boolean {
    return isNaN(number)
  },
}
