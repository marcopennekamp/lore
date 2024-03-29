# Test

This folder contains test cases, ranging from the practical to focused language feature tests in `features`. All tests are automatically executed by our test suite (see `test.sh` in the root folder of the project).

Here is an **overview** of the most important top-level directories:

- [Language](language): Lore language tests, focused on testing specific areas of the language. The code that has to be written for these tests often doesn't follow best practices. Hence, these tests aren't idiomatic examples and best ignored outside of language development.
- [Lessons](lessons): Single-file, simple Lore programs that follow a specific objective.
- [Examples](examples): Other single-file Lore programs, without the level of focus of a lesson.

If you want to have a first look at some Lore programs, consider diving into the [lessons](lessons).

These **complex test cases** deserve their own directories:

- [Calculator](calculator): A reverse Polish notation parser and evaluator. The calculator supports positive integers and basic arithmetic operations.
- [Combat](combat): A turn-based combat simulation inspired by RPGs.
