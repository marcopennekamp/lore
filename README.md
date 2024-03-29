# Lore

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, a static type system with sum and intersection types, and a mix of functional and imperative programming. Take a look at it:

```
use lore.list.map

func hello(String | Int): String
func hello(name: String): String = 'Hello, $name.'
func hello(id: Int): String = 'Hello, anonymous #$id.'

func main(): [String] = ['world', 5, 'marco', 'console', 42] |> map(hello)
```

This is [hello_name.lore](../test/lessons/hello_name.lore), a hello world example making use of Lore's multiple dispatch. Depending on whether `hello`'s argument is a `String` or an `Int`, it either returns `'Hello, name.'` or `'Hello, anonymous #id.'`. The program results in a list of strings:

```
['Hello, world.', 'Hello, anonymous #5.', 'Hello, marco.', 'Hello, console.', 'Hello, anonymous #42.']
```

To learn more about Lore, read the [**overview**](specification/README.md) of our specification.



### Project Overview

This repository is divided into the following parts:

- [**Specification:**](specification) The Lore specification, which is a user-facing reference manual rather than a technical specification.
- [**Compiler:**](compiler) The compiler, written in Scala, translates a Lore program to Poem bytecode, which is the custom bytecode format of the Lore VM.
- [**VM:**](vm) The VM executes Poem bytecode produced by the compiler. Crucially, the VM also implements Lore's type system, which is required for run-time multiple dispatch. It supports multi-functions natively and is thus a multiple dispatch VM.
- [**Pyramid:**](pyramid) This is the Lore standard library, which defines core functions expected by the compiler, functions to work with built-in values such as lists, maps and strings, and additional types such as `Option`s.
- [**Test:**](test) Functional tests, which are comprised of many smaller Lore programs. While the purpose of these tests is to verify the correctness of Lore's compiler and VM, the Lore code can also be used as a repository of examples for learners, especially [test/lessons](test/lessons).
- **LSP:** An LSP language server and VSCode extension. Neither are currently up-to-date as Lore evolves, but they will be supported in the future to offer native IDE integration.



### State of the Project

I've been working on Lore originally since 2017, more actively since April 2020, and full-time since June 2021. A first milestone version of the language is almost ready, with only a handful of features missing and some minor design choices to be reconsidered.



### Can I try it?

I haven't yet invested time into making Lore ready for the public. Hence, I'm not providing any compiler/runtime builds at this time.

You are welcome to clone the repository and make it work yourself, however. Here is a short guideline:

1. You will need [SBT](https://www.scala-sbt.org) to build and execute the compiler, and [Nim](https://nim-lang.org/install.html) to build the VM.
2. In the project's `<root>` folder, execute the command `sbt`, which starts an SBT session. I'd suggest compiling the [test/lessons/hello_name.lore](test/lessons/hello_name.lore) example first, which is in many ways Lore's hello world. Inside the SBT console, execute the following command: `compiler/run compile test/lessons/hello_name.lore`. 
3. The compilation should finish with a message `[info] Compilation was successful.`. The compiler will have created a file `lore_target/binary.poem`, which is the bytecode file that the VM can execute.
4. `cd` to the folder `vm` and execute the following command: `./run.sh vm.nim run ../lore_target/binary.poem lessons.hello_name.main`. This executes the Lore program you just compiled.
   - You might have to `chmod` the `run.sh` file for execution permissions. 
   - The argument `lessons.hello_name.main` specifies the entry multi-function for the VM, which is `*.main` for `hello_name.lore` and all other lessons.
5. If everything goes well, the result reported by the VM should be: `[Hello, world., Hello, anonymous #5., Hello, marco., Hello, console., Hello, anonymous #42.] :: [String]`.
6. From here on out, you might want to compile additional examples or write your own Lore programs. Here are some tips:
   - The compiler accepts multiple files and folders. For example, executing `compiler/run compile test/combat` in SBT will compile all `.lore` files of the `test/combat` example.
   - The folder `pyramid` contains all standard types and functions you can use. As there is currently no HTML-based documentation, you're encouraged to read the source code for documentation. Notice how the test examples import various types and functions from the module `lore`. You will have to do the same in your own code.
   - The parser's error reporting is currently very bad, as Lore uses a parser combinator instead of a handwritten parser. It frequently reports errors on the wrong line with cryptic error messages. Don't get discouraged if you encounter such an error! Try to comment out parts of the code until it compiles to single out the issue. If you're stuck, don't hesitate to contact me (info below).

I have only tested this workflow on Linux and you might run into additional problems if you're using another operating system. If you have any questions or issues, feel free to email me at my [Github profile's](https://github.com/marcopennekamp) listed email address, or contact `Marco#9733` on Discord.



### Functional Tests

One part of this project is Lore's functional testing solution. In the folder [`test`](test), you will find Lore programs contained in single files, or in some cases across multiple files (`test/calculator` and `test/combat`). You can compile and run all tests by executing `test.sh`.

Each test program defines a number of specs which can be executed by the Lore VM as tests and as benchmarks (depending on whether the spec is marked as a test, benchmark, or both). These specs use assertions to ensure that each test results in the expected outcome. Each file can additionally use types and functions from Pyramid.

This solution allows us to automate "real-world" testing of Lore programs. By adding specs, we can ensure that specific features of the language, compiler, and VM work as intended. It helps to find software regressions (as long as tests are added when fixing an issue) and keeps the language stable. As each test requires the work of the compiler and VM, Lore's functional tests are whole-system tests.



### Native Image

A native executable version of the Lore compiler can be built from an assembled JAR using GraalVM native-image.

To generate new versions of the `compiler/native-image/*` configuration files, execute the following command from a folder that contains a `lore.build.json` file:

```shell
java -agentlib:native-image-agent=config-output-dir=graal-config/ -jar compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar build
```
