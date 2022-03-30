# Lore

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, a static type system with sum and intersection types, and a mix of functional and imperative programming.

To learn more about the language, read the [**overview**](specification/overview.md) of our specification.



### Project Overview

This repository is divided into the following parts:

- **Specification:** The Lore specification. We have taken care not to make the specification overly technical. It is, mostly, a user-facing description of the language and can be read as a guide to the language. The `technical` subfolder goes into more detail that might not be useful to average language users.
- **Compiler:** The compiler, written in Scala, translates a Lore program to Poem bytecode, which is the custom bytecode format of the Lore VM.
- **VM:** The VM executes Poem bytecode produced by the compiler. Crucially, the VM also implements Lore's type system, which is required for run-time multiple dispatch. It supports multi-functions natively and is thus a multiple dispatch VM.
- **Pyramid:** This is the Lore standard library, which defines core functions expected by the compiler, functions to work with values such as lists, maps and strings, and additional types such as Options. The name refers to the Pyramid Texts, which are one of the oldest extant bodies of (religious) lore in the world.
- **Test:** Functional tests, which are comprised of many smaller Lore programs. This is the de-facto place to look at existing Lore code, such as [test/lessons](test/lessons) and [test/combat](test/combat).



### State of the Project

I've been working on Lore originally since 2017, more actively since April 2020, and full-time since June 2021. A first milestone version of the language is almost ready, with only a handful of features missing and some minor design choices to be reconsidered. 

Lately, I've been in the process of replacing the Javascript/Typescript runtime with a custom virtual machine written in Nim. The VM is feature-complete, barring some important optimizations, and can already execute all functional tests in the `test` folder correctly. My next goals are to clear any outstanding TODOs around the change and to automate functional tests again, as they are still assuming a Typescript runtime.



### Can I try it?

I haven't yet invested time into making Lore ready for the public. Hence, I'm not providing any compiler/runtime builds at this time.

You are welcome to clone the repository and make it work yourself, however. Here is a short guideline:

1. You will need [SBT](https://www.scala-sbt.org) to build and execute the compiler, and [Nim](https://nim-lang.org/install.html) to build the VM.
2. In the project's `<root>` folder, execute the command `sbt`, which starts an SBT session. I'd suggest compiling the [test/lessons/hello-name.lore](test/lessons/hello-name.lore) example first, which is in many ways Lore's hello world. Inside the SBT console, execute the following command: `compiler/run compile test/lessons/hello-name.lore`. 
3. The compilation should finish with a message `[info] Compilation was successful.`. The compiler will have created a file `lore_target/binary.poem`, which is the bytecode file that the VM can execute.
4. `cd` to the folder `vm` and execute the following command: `./run.sh vm.nim ../lore_target/binary.poem test`. This executes the Lore program you just compiled.
   - You might have to `chmod` the `run.sh` file for execution permissions. 
   - The argument `test` specifies the entry multi-function for the VM, which is `test` for `hello-name.lore` and all other test programs.
5. If everything goes well, the result reported by the VM should be: `[Hello, world., Hello, anonymous #5., Hello, marco., Hello, console., Hello, anonymous #42.] :: [String]`.
6. From here on out, you might want to compile additional examples or write your own Lore programs. Here are some tips:
   - The compiler accepts multiple files and folders. For example, executing `compiler/run compile test/combat` in SBT will compile all `.lore` files of the `test/combat` example.
   - The folder `pyramid` contains all standard types and functions you can use. As there is currently no HTML-based documentation, you're encouraged to read the source code for documentation. Notice how the test examples import various types and functions from the module `lore`. You will have to do the same in your own code.
   - The parser's error reporting is currently very bad, as Lore uses a parser combinator instead of a handwritten parser. It frequently reports errors on the wrong line with cryptic error messages. Don't get discouraged if you encounter such an error! Try to comment out parts of the code until it compiles to single out the issue. If you're stuck, don't hesitate to contact me (info below).

I have only tested this workflow on Linux and you might run into additional problems if you're using another operating system. If you have any questions or issues, feel free to email me at my [Github profile's](https://github.com/marcopennekamp) listed email address, or contact `Marco#9733` on Discord.



### Functional Tests

*Note: Functional test automation is not currently up-to-date, as it hasn't been updated for the VM changes.*

One part of this project is Lore's functional testing solution. In the folder `test` (a direct child of the project root), you will find Lore programs contained in single files or across multiple files in some cases (`test/calculator` and `test/combat`). Each test program can additionally use types and function from Pyramid.

Each Lore program in this folder defines a function `test()`, which may return any kind of value. Additionally, we define Deno tests that compile the programs, execute the test functions, and verify the return values. When running `test.sh` (in the project root), the Lore compiler is freshly assembled into a native image for compilation speed (from a JAR) and subsequently all Deno tests (`*.test.ts`) in the `test` folder are executed.

This solution allows us to automate "real-world" testing of Lore programs. By adding such test programs, we can ensure that specific features of the language and compiler work as intended. It helps to find software regressions (as long as tests are added when fixing an issue) and keeps the language stable. As each test requires the work of the compiler and runtime, our functional tests are whole-system tests.



### Native Image

A native executable version of the Lore compiler can be built from an assembled JAR using GraalVM native-image. See `test.sh` for a sample use of the command.

To generate new versions of the `compiler/native-image/*` configuration files, execute the following command from a folder that contains a `lore.build.json` file:

```shell
java -agentlib:native-image-agent=config-output-dir=graal-config/ -jar compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar build
```
