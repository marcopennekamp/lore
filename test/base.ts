import { assert } from 'https://deno.land/std/testing/asserts.ts'

/*+
 * A test utility that allows functional testing of individual Lore source files. The files can be compiled and then
 * a test function can be executed and its return value can be judged. You need to execute these tests with --allow-run
 * or --allow-all because the tests have to invoke the Lore compiler via a shell command.
 *
 * Please run the test from the <lore root>/test directory so that the current working directory of the compilation
 * command is correct.
 */
export const LoreTest = {
  async run(...paths: string[]): Promise<any> {
    await LoreTest.compile(...paths)
    return LoreTest.execute()
  },

  /**
   * Compiles a given test file to <lore root>/lore-program.js and asserts that the compilation was successful.
   *
   * @param paths All paths to the test files, with <lore root>/test as the base directory and without a file extension.
   *              For example, giving 'return/simple' as the test path would result in <lore root>/test/return/simple.lore.
   */
  async compile(...paths: string[]): Promise<void> {
    const process = Deno.run({
      cmd: ['java', '-jar', 'lore.jar', '..', ...paths.map(path => `test/${path}`)],
      stdout: 'piped',
    })

    const messages = await stdoutMessages(process)

    process.close()

    // We judge the success of the compilation based on the following line being included in the standard output:
    //    [success] Compilation was successful.
    // This line is always and only posted by the Lore compiler when compilation is successful.
    const isSuccessful = messages.includes('[\u001b[32msuccess\u001b[0m] Compilation was successful.')
    const failMessage = `The given Lore source code in ${paths.join(', ')} cannot be compiled. Compiler output:\n` + messages.join('\n')
    assert(isSuccessful, failMessage)
  },

  /**
   * Execute the Lore program's test() function in a new process, returning the function's return value deserialized
   * from the process's JSON output.
   */
  async execute(): Promise<any> {
    const process = Deno.run({
      cmd: ['deno', 'run', 'execute.ts'],
      stdout: 'piped',
    })

    const messages = await stdoutMessages(process)

    process.close()

    return JSON.parse(messages[0])
  },
}

async function stdoutMessages(process: Deno.Process): Promise<Array<string>> {
  const output = new TextDecoder().decode(await process.output())
  return output.split('\n').map(line => line.trim()).filter(line => !!line)
}
