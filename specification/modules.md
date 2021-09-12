# Modules

**Modules** separate top-level definitions into logical units. Modules are *not* restricted to files or library bounds: you can declare a module anywhere, regardless of the source file's name. You can split a module across many files, and declare multiple modules in the same file. You can even extend a module first declared in a library with additional specialized functions, types, global variables, and modules. Modules can be nested arbitrarily.

A module can be **declared** in two ways:

1. **At the top of a file**, a declaration `module* Name` will include all other declarations in the file into the module called `Name`. Each file can only have a single top module declaration.
2. With a **block scope**, a module can be declared at any declaration-level position in the file. All declarations inside the module's block will be put into the module.

A declaration inside a module is called a **module member**. Module members may be accessed with the member access notation: `Module.member`. Such an expression is also called a **module path**.

A module path can also be used as the name in a module declaration, which effectively creates a nested module. A module declaration `module my_project.utils.Timer` creates a module `Timer` inside a module `utils`, which in turn is inside a module `my_project`.

Declarations outside a module are put into the implicit **root module**, which all first-level modules are also part of. The module `my_project` defined in the preceding paragraph would be part of the root module.

The requirement for the `do` is purely due to parsing ambiguities that would arise otherwise. Without the `do`, the parser might have to read the whole file to find the `end` that closes the block module declaration which the parser would first assume to be a top module declaration. This will eventually be removed once we introduce significant indentation.

###### Example (Declaration)

```
module Name

func foo(): String = 'Foo'

module Name2 do
  func foo(): Int = 17
end
```

The module `Name2` is located inside the surrounding module `Name`. Without any `use` qualifiers, the functions can be accessed using `Name.foo` and `Name.Name2.foo`. Both `foo` multi-functions are completely distinct, as their full names are different. Hence we each have a multi-function that contains only one function instance.

###### Example (Access)

```
lore.Enum.map([1, 2, 3], lore.Math.increment)      // --> [2, 3, 4]
lore.String.concat(['Hello', ', ', 'world', '!'])  // --> 'Hello, world!'
```



### Use Declarations

The `use` declaration can be used to **simplify member names** at their point of use. It is also called an **import**. It has three flavors:

1. **Simple:** Use a single member.

   ```
   use lore.Enum.map
   ```

2. **Multiple:** Use multiple members of the same module.

   ```
   use lore.Enum.[map, flat_map]
   ```

3. **Wildcard:** Use all members of a module.

   ```
   use lore.Enum._
   ```

The `use` declaration can only be placed at the beginning of a module declaration. As fragments are also modules even without a top module declaration, `use` can also stand at the beginning of a file. If a name is imported multiple times (from potentially different sources), the last `use` wins.

###### Example

```
// Simple:
use lore.Enum.map
use lore.Math.increment
[1, 2, 3] |> map(increment)

// Multiple:
use lore.[Enum, Math]
[1, 2, 3] |> Enum.map(Math.increment)

// Wildcard:
use lore.Math._
let x1 = (-b + sqrt(pow(b, 2) - product([4, a, c]))) / (2 * a)
```



### Name Precedence

**TODO (modules):** Rewrite this for easier digestion and add a code example spanning at least two fragments.

If a module member of the current module is declared as `name`, an import `use name` will have no effect in that module, because **local declarations supersede imports**. However, if `name` is declared in some other fragment, and the current fragment has an import `use name`, the import is preferred. This is necessary so that adding a name to a module doesn't break existing code in other fragments. And finally, a local declaration or import in a parent module (through nesting) supersedes non-local module members. Hence, we have the following **name precedence:**

```
local declarations > imports (> parent local declarations > parent imports)* > module members (> parent module members)*
```

If a module is nested twice, the inner parent has precedence over the outer parent, and so on. This is signified by the `*` in the above inequation.



### Companion Modules

A type and a module may share a name. A module that bears the same name as a type is called a **companion module**. This module is *expected*, by convention, to contain functions for working with the type. For example, the `Option` type has a companion module `Option` that contains functions for working with options, such as `Option.get`.

###### Example

```
module animals

trait Animal

module Animal do
  func breed(mother: Animal, father: Animal): Option[Animal] = None
end
```



### Visibility

Module members have two modes of **visibility:**

- **Private** members may only be accessed from the same module.
- **Public** members may be accessed anywhere. 

All module members are **public by default**. Private members are declared by prefixing their declaration keywords with a `-` sign: `-trait`, `-function`, and so on. If a single function definition is private, all function definitions of the owning **multi-function** must be private as well. This has to be applied *explicitly* so that multi-functions cannot be unexpectedly private.

###### Example

```
module my_project

trait Foo

module Foo do
  -struct Implementation extends Foo
    mut counter: Int
  end
  
  func fresh(): Foo = Implementation(0)
  
  func get_and_increment(Foo): Int
  func get_and_increment(foo: Implementation): Int = do
    let result = foo.counter
    increment(foo)
    result
  end
  
  -act increment(foo: Implementation)
    foo.counter += 1
  end
end
```



### Naming Conventions

Modules are essentially used in two ways, which determines their naming convention:

1. Modules that **contain functions and global variables** are usually named in upper camel case. Often they will be used explicitly, so that functions from multiple modules don't clash. A good example is the module `lore.Enum`. Its functions will often be used together with the module name, e.g. `Enum.map([1, 2, 3], x => x + 1)` and `Enum.flatten(lists)`. Sometimes these modules are also companion modules. 
   - An exception to this convention is when functions or global variables are intended to be used without the qualifying module name. In such cases the module is best understood as a namespace.
2. Modules that only contain types and other modules are also informally known as **namespaces**. They are written in snake case, such as the `lore` namespace defined by the Pyramid standard library.



### TODOs

- **Use anywhere:** The `use` declaration should be usable anywhere (inside expressions as a top-level expression) for more fine-grained control of names.
  - To support this, we can support imports in *local scopes*.
- **Aliases:** The `use` declaration should allow the programmer to rename a given symbol.
- **Private problems:** Let's say we declare a private function `f` in a module defined by Pyramid, for example `lore.Enum`. When the language user (idiomatically) extends the `Enum` module with additional functions, for example to implement `Enum.map`  for their custom collection type, they are able to see the private function `f` from within their extension of the Enum module. This is a problem, right? Shouldn't private functions and types thus only be visible inside the same fragment? Or do we need an additional mechanism, for example `private[module]` for module privacy and `private[fragment]` for fragment privacy?

