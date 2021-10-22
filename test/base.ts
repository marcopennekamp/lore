import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { JsonConversion } from '../runtime/src/lore/runtime/utils/JsonConversion.ts'

/*+
 * A test utility that allows functional testing of individual Lore source files. The files can be compiled and then
 * a test function can be executed and its return value can be judged. You need to execute these tests with --allow-run
 * or --allow-all because the tests have to invoke the Lore compiler via a shell command.
 *
 * Please run the test from the `<lore root>/test` directory so that the current working directory of the compilation
 * command is correct.
 */
export const LoreTest = {
  async run(...paths: string[]): Promise<any> {
    const outputFile = 'target/target.js'
    await LoreTest.compile(paths, outputFile)
    return LoreTest.execute(outputFile)
  },

  /**
   * Compiles a given test file to `<lore root>/lore-program.js` and asserts that the compilation was successful.
   *
   * @param paths All paths to the test files or directories, with `<lore root>/test` as the base directory. For
   *              example, giving 'features/syntax/return.lore' as the test path would result in
   *              `<lore root>/test/features/syntax/return.lore`.
   * @param outputFile The unique Javascript file that the generated code is written to.
   */
  async compile(paths: string[], outputFile: string): Promise<void> {
    const process = Deno.run({
      cmd: ['./target/lore', 'compile', '--sdk', '..', '--target', outputFile, '--no-prettier', ...paths],
      stdout: 'piped',
    })
    await process.status()

    const messages = await stdoutMessages(process)

    process.close()

    // We judge the success of the compilation based on the following line prefix being included in the standard output:
    //    [info] Compilation was successful.
    // This line is always and only posted by the Lore compiler when compilation is successful. It may contain
    // additional information such as the total time taken, so we are checking for the prefix.
    const isSuccessful = messages.some(message => message.startsWith('[info] Compilation was successful.'))
    const failMessage = `The given Lore source code in ${paths.join(', ')} cannot be compiled. Compiler output:\n` + messages.join('\n')
    assert(isSuccessful, failMessage)
  },

  /**
   * Execute the Lore program's test() function in a new process, returning the function's return value deserialized
   * from the process's JSON output.
   */
  async execute(outputFile: string): Promise<any> {
    const process = Deno.run({
      cmd: ['deno', 'run', '--allow-read', 'execute.ts', outputFile],
      stdout: 'piped',
      stderr: 'piped',
    })
    await process.status()

    let result: any = undefined
    const errors = await stderrMessages(process)
    // Has to be awaited here, because the piped stdout needs to be closed.
    const messages = await stdoutMessages(process)
    if (errors.length) {
      result = errors[0]
    } else {
      result = JsonConversion.toLoreFromString(messages[messages.length - 1])
    }

    process.close()

    return result
  },
}

async function stdoutMessages(process: Deno.Process): Promise<Array<string>> {
  return messages(await process.output())
}

async function stderrMessages(process: Deno.Process): Promise<Array<string>> {
  return messages(await process.stderrOutput())
}

function messages(binaryOutput: Uint8Array): Array<string> {
  const output = new TextDecoder().decode(binaryOutput)
  return output.split('\n').map(line => line.trim()).filter(line => !!line)
}
