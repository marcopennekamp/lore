export const StringFunctions = {
  length(string: string): number {
    return string.length
  },

  at(string: string, position: number): string {
    return string.charAt(position)
  },
}
