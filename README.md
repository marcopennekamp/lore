# Lore

**Lore** is a general-purpose programming language featuring multi-functions, a type-safe entity-component system, and intersection and semantic types.

To learn more about the language, read the [**introduction**](spec/01-introduction.md) to our spec.



### State of the Project

In April 2020, I finished a specification for a **"minimum viable language"** (MVL). My goal is to demonstrate the core concepts of Lore with a usable, minimal propotype compiler and run-time. I am currently implementing the compiler, which will transpile to Javascript because it's popular, ubiquitous, easy to interface with, and has an excellent VM/JIT implementation. Lore will also have a run-time environment due to the nature of multi-functions: multiple dispatch needs to be decided at run-time, based on actual argument types instead of declared parameter types.

This is a **hobby project** I'm working on infrequently, although I have picked up the pace since April 2020. I'd be happy just getting the MVL implemented so that I can demonstrate the ideas that went into Lore not just theoretically, but with practical, usable, changeable examples.