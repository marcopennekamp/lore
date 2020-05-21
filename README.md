# Lore

**Lore** is a general-purpose programming language featuring multi-functions, a type-safe entity-component system, and intersection and semantic types.

To learn more about the language, read the [**introduction**](spec/01-introduction.md) to our spec.



### Project Overview

This repository is divided into the following **parts:**

- **Spec:** The Lore specification. Parts of the specification heavily rely on inline TeX, which is not supported by Github. If you want to read the spec nicely formatted, please shoot me an email.
- **Examples:** Examples of Lore code, some of which can be compiled (or even run) today. Others are in a conceptual state. This directory also contains Lore test sources.
- **Common:** This is the common code that is shared by the compiler and the runtime. Currently, this mostly concerns the type 
- **Compiler:** The compiler, written in Scala, translates a Lore program to a Javascript file.
- **Runtime:** The runtime, also written in Scala, is compiled using Scala.js, which allows us to write most of the runtime in Scala and, crucially, share code with the compiler. This is super useful since we need type information at runtime. It would've been a shame if we had to write that twice (especially the subtyping algorithm). So thanks to Scala.js for making this possible!
- **Pyramid:** This is the Lore standard library, which defines functions to work with Lore values, such as lists, maps and strings. You don't HAVE to use it, frankly, but currently it's baked into the compiler and you will want to use it if you have any sanity left in you. I'm just saying that the compiler doesn't rely on the existence of these functions and it's possible to plug your own standard library into the language. The name refers to the Pyramid Texts, which are one of the oldest extant bodies of lore in the world, in this case religious lore.



### State of the Project

In April 2020, I finished a specification for a **"minimum viable language"** (MVL). My goal is to demonstrate the core concepts of Lore with a usable, minimal propotype compiler and run-time. I am currently implementing the compiler, which will transpile to Javascript because it's popular, ubiquitous, easy to interface with, and has an excellent VM/JIT implementation. Lore will also have a run-time environment due to the nature of multi-functions: multiple dispatch needs to be decided at run-time, based on actual argument types instead of declared parameter types.

This is a **hobby project** I'm working on infrequently, although I have picked up the pace since April 2020. I'd be happy just getting the MVL implemented so that I can demonstrate the ideas that went into Lore not just theoretically, but with practical, usable, changeable examples.