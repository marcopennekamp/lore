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

function foo(): String = 'Foo'

module Name2 {
  function foo(): Int = 17
}
```

The module `Name2` is located inside the surrounding module `Name`. Without any `use` qualifiers, the functions can be accessed using `Name.foo` and `Name.Name2.foo`. Both `foo` multi-functions are completely distinct, as their full names are different. Hence we each have a multi-function that contains only one function instance.

###### Example (Access)

```
lore.Enum.map([1, 2, 3], lore.Math.increment)      // --> [2, 3, 4]
lore.String.concat(['Hello', ', ', 'world', '!'])  // --> 'Hello, world!'
```



### Imports

**TODO**



### Companion Modules

A type and a module may share a name. A module that bears the same name as a type is called a **companion module**. This module is *expected*, by convention, to contain functions for working with the type. For example, the `Option` type has a companion module `Option` that contains functions for working with options, such as `Option.get`.



### Visibility

Module members have two modes of **visibility:** private and public. **Private** members may only be accessed from the same module. All types, functions, and global variables are private by default. **Public** members may be accessed anywhere. All modules are public by default.

If a single function definition is public, all function definitions of the owning **multi-function** must be public as well. This has to be applied *explicitly* so that multi-functions cannot accidentally become public.

###### Example

```
module my_project

public trait Foo

module Foo {
  struct Implementation extends Foo { mut counter: Int }
  
  public function fresh(): Foo = Implementation(0)
  
  public function getAndIncrement(foo: Foo): Int
  public function getAndIncrement(foo: Implementation): Int = {
    let result = foo.counter
    foo.counter += 1
    result
  }
}
```



### Naming Conventions

Modules are essentially used in two ways, which determines their naming convention:

1. Modules that **contain functions and global variables** are usually named in upper camel case. Most of the time, they will be used explicitly, so that functions from multiple modules don't clash. Very often, these modules are also companion modules. A good example is the module `lore.Enum`. Its functions will often be used together with the module name, e.g. `Enum.map([1, 2, 3], x => x + 1)` and `Enum.flatten(lists)`.
   - An exception to this convention is when functions or global variables are intended to be used without the qualifying module name. In such cases the module is best understood as a namespace.
2. Modules that only contain types and other modules are also informally known as **namespaces**. They are written in snake case, such as the `lore` namespace defined by the Pyramid standard library.

