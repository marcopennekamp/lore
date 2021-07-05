# Lore

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, structural typing enabling component-based programming, and intersection and semantic types.

To learn more about the language, read the [**overview**](specification/overview.md) of our specification.



### Project Overview

This repository is divided into the following **parts:**

- **Spec:** The Lore specification. We have taken care not to make the specification overly technical. It is, mostly, a user-facing description of the language and can be read as a guide to the language. The `technical` subfolder goes into more detail that might not be useful to average language users.
- **Examples:** Examples of Lore code, some of which can be compiled (or even run) today. Others are in a conceptual state. This directory also contains Lore test sources.
- **Compiler:** The compiler, written in Scala, translates a Lore program to a Javascript file.
- **Runtime:** The runtime, written in Typescript, contains utilities that are required to run compiled Lore code. These utilities include run-time value representation and manipulation, data structures, and crucially our type system, which is needed at run-time for multiple dispatch.
- **Pyramid:** This is the Lore standard library, which defines functions to work with Lore values, such as lists, maps and strings. Some core functions declared by Pyramid are used by the compiler, but you can omit using Pyramid and roll your own standard library. The name refers to the Pyramid Texts, which are one of the oldest extant bodies of (religious) lore in the world.



### State of the Project

Originally since 2017, but more actively since April 2020, I've been working towards the implementation of a **"minimum viable language"** (MVL). My short-term goal is to create a solid basis of core concepts which I can successively build upon.



### Functional Tests

One part of this project is Lore's functional testing solution. In the folder `test` (a direct child of the project root), you will find Lore programs contained in single files. As is currently the default behavior of the compiler, each program can additionally use Pyramid standard types and functions.

Each Lore program in this folder defines a function `test()`, which may return any kind of value. Additionally, we define Deno tests that compile the programs, execute the test functions, and verify the return values. When running `test.sh` (in the project root), the Lore compiler is freshly assembled into a JAR and subsequently all Deno tests in the `test` folder are executed.   

This solution allows us to automate "real-world" testing of Lore programs. By adding such test programs, we can ensure that specific features of the language and compiler work as intended. It helps finding software regressions (as long as tests are added when fixing an issue) and helps keep the language stable. As each test requires the work of the compiler and runtime, our functional tests are whole-system tests.



### Native Image

A native executable version of the Lore compiler can be built from an assembled JAR using GraalVM native-image. See `test.sh` for a sample use of the command.

To generate new versions of the `compiler/native-image/*` configuration files, execute the following command from a folder that contains a `lore.build.json` file:

```shell
java -agentlib:native-image-agent=config-output-dir=graal-config/ -jar compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar build
```
