# Modules

**Modules** separate top-level definitions into logical units. Modules are *not* restricted to files or library bounds: you can declare a module anywhere, regardless of file name. You can split the same module across many files, and declare multiple modules in the same file. You can even extend a module first declared in a library with additional specialized functions, types, global variables, and modules. Modules can be nested arbitrarily.

A module can be **declared** in two ways:

1. **At the top of a file**, a declaration `module Name` will include all other declarations in the file into the module called `Name`. Each file can only have a single top module declaration.
2. With a **block scope**, a module can be declared at any declaration-level position in the file. All declarations inside the module's block will be put into the module.

A declaration inside a module is called a **module member**. Module members may be accessed with the member access notation: `Module.member`. Such an expression is also called a **module path**.

A module path can also be used as the name in a module declaration, which effectively creates a nested module. A module declaration `module my_project.utils.Timer` creates a module `Timer` inside a module `utils`, which in turn is inside a module `my_project`.

###### Example (Declaration)

```
module Name

func foo(): String = 'Foo'

module Name2 {
  func foo(): Int = 17
}
```

The module `Name2` is located inside the surrounding module `Name`. Without any `use` qualifiers, the functions can be accessed using `Name.foo` and `Name.Name2.foo`. Both `foo` multi-functions are completely distinct, as their full names are different. Hence we each have a multi-function that contains only one function instance.

###### Example (Access)

```
lore.Enum.map([1, 2, 3], lore.Math.increment)      // --> [2, 3, 4]
lore.String.concat(['Hello', ', ', 'world', '!'])  // --> 'Hello, world!'
```



### Use Declarations

The `use` declaration can be used to **simplify member names** at their point of use. It has three flavors:

1. **Simple:** Use a single member.

   ```
   use lore.Enum.map
   ```

2. **Multiple:** Use multiple members of the same module.

   ```
   use lore.Enum.[map, flatMap]
   ```

3. **Wildcard:** Use all members of a module.

   ```
   use lore.Enum._
   ```

The `use` declaration can only be placed at the top of a file, below the optional top module declaration.

###### Example

```
// Simple:
use lore.Enum.map
use lore.Math.increment
map([1, 2, 3], increment)

// Multiple:
use lore.[Enum, Math]
Enum.map([1, 2, 3], Math.increment)

// Wildcard:
use lore.Math._
let x1 = (-b + sqrt(pow(b, 2) - product([4, a, c]))) / (2 * a)
```



### Companion Modules

A type and a module may share a name. A module that bears the same name as a type is called a **companion module**. This module is *expected*, by convention, to contain functions for working with the type. For example, the `Option` type has a companion module `Option` that contains functions for working with options, such as `Option.get`.

###### Example

```
module animals

trait Animal

module Animal {
  func breed(mother: Animal, father: Animal): Option[Animal] = None()
}
```



### Visibility

Module members have two modes of **visibility:**

- **Private** members may only be accessed from the same module.
- **Public** members may be accessed anywhere. 

All module members are **public by default**. Private members are declared by prefixing their declaration keywords with a `-` sign: `-trait`, `-function`, and so on. If a single function definition is private, all function definitions of the owning **multi-function** must be private as well. This has to be applied *explicitly* so that multi-functions cannot accidentally become private.

###### Example

```
module my_project

trait Foo

module Foo {
  -struct Implementation extends Foo { mut counter: Int }
  
  func fresh(): Foo = Implementation(0)
  
  func getAndIncrement(foo: Foo): Int
  func getAndIncrement(foo: Implementation): Int = {
    let result = foo.counter
    increment(foo)
    result
  }
  
  -action increment(foo: Implementation) {
    foo.counter += 1
  }
}
```



### Naming Conventions

Modules are essentially used in two ways, which determines their naming convention:

1. Modules that **contain functions and global variables** are usually named in upper camel case. Most of the time, they will be used explicitly, so that functions from multiple modules don't clash. Very often, these modules are also companion modules. A good example is the module `lore.Enum`. Its functions will often be used together with the module name, e.g. `Enum.map([1, 2, 3], x => x + 1)` and `Enum.flatten(lists)`.
   - An exception to this convention is when functions or global variables are intended to be used without the qualifying module name. In such cases the module is best understood as a namespace.
2. Modules that only contain types and other modules are also informally known as **namespaces**. They are written in snake case, such as the `lore` namespace defined by the Pyramid standard library.



### TODOs

- **Use anywhere:** The `use` declaration should be usable anywhere (within any kind of block) for more fine-grained control of names.
- **Aliases:** The `use` declaration should allow the programmer to rename a given symbol.
- **Private problems:** Let's say we declare a private function `f` in a module defined by Pyramid, for example `lore.Enum`. When the language user (idiomatically) extends the `Enum` module with additional functions, for example to implement `Enum.map`  for their custom collection type, they are able to see the private function `f` from within their extension of the Enum module. This is a problem, right? Shouldn't private functions and types thus only be visible inside the same fragment? Or do we need an additional mechanism, for example `private[module]` for module privacy and `private[fragment]` for fragment privacy?

