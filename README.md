# Lore

**Lore** is a general-purpose programming language featuring multi-functions, a type-safe entity-component system, and intersection and semantic types.

To learn more about the language, read the [**overview**](specification/overview.md) of our specification.



### Project Overview

This repository is divided into the following **parts:**

- **Spec:** The Lore specification. We have taken care not to make the specification overly technical. It is, mostly, a user-facing description of the language and can be read as a guide to the language. The `technical` subfolder goes into more detail that might not be useful to average language users.
- **Examples:** Examples of Lore code, some of which can be compiled (or even run) today. Others are in a conceptual state. This directory also contains Lore test sources.
- **Compiler:** The compiler, written in Scala, translates a Lore program to a Javascript file.
- **Runtime:** The runtime, written in Typescript, contains utilities that are required to run compiled Lore code. These utilities include run-time value representation and manipulation, data structures, and crucially our type system, which is needed at run-time for multiple dispatch.
- **Pyramid:** This is the Lore standard library, which defines functions to work with Lore values, such as lists, maps and strings. Some core functions declared by Pyramid are used by the compiler, but you can omit using Pyramid and roll your own standard library. The name refers to the Pyramid Texts, which are one of the oldest extant bodies of (religious) lore in the world.



### State of the Project

In April 2020, I finished a specification for a **"minimum viable language"** (MVL). My goal is to demonstrate the core concepts of Lore with a usable, minimal propotype compiler and run-time. I am currently implementing the compiler, which will transpile to Javascript because it's popular, ubiquitous, easy to interface with, and has an excellent VM/JIT implementation. Lore will also have a run-time environment due to the nature of multi-functions: multiple dispatch needs to be decided at run-time, based on actual argument types instead of declared parameter types.

This is a **hobby project** I'm working on infrequently, although I have picked up the pace since April 2020. I'd be happy just getting the MVL implemented so that I can demonstrate the ideas that went into Lore not just theoretically, but with practical, usable, changeable examples.



### Functional Tests

One part of this project is Lore's functional testing solution. In the folder `test` (a direct child of the project root), you will find Lore programs contained in single files. As is currently the default behavior of the compiler, each program can additionally use Pyramid standard types and functions.

Each Lore program in this folder defines a function `test()`, which may return any kind of value. Additionally, we define Deno tests that compile the programs, execute the test functions, and verify the return values. When running `test.sh` (in the project root), the Lore compiler is freshly assembled into a JAR and subsequently all Deno tests in the `test` folder are executed.   

This solution allows us to automate "real-world" testing of Lore programs. By adding such test programs, we can ensure that specific features of the language and compiler work as intended. It helps finding software regressions (as long as tests are added when fixing an issue) and helps keep the language stable. As each test requires the work of the compiler and runtime, our functional tests are whole-system tests.
