# Modules

**Modules** separate top-level definitions into logical units. Modules are *not* restricted to files or library bounds: you can declare a module anywhere, regardless of the source file's name. You can split a module across many files, and declare multiple modules in the same file. You can even extend a module first declared in a library with additional specialized functions, types, global variables, and modules. Modules can be nested arbitrarily.

A module can be **declared** in two ways:

1. **At the top of a file**, a declaration `module Name` will include all other declarations in the file into the module called `Name`. Each file can only have a single top module declaration.
2. With a **block scope**, a module can be declared at any declaration-level position in the file. All declarations inside the module's block will be put into the module.

A declaration inside a module is called a **module member**. Module members may be accessed with the member access notation: `Module.member`. Such an expression is also called a **module path**.

A module path can also be used as the name in a module declaration, which effectively creates a nested module. A module declaration `module my_project.utils.Timer` creates a module `Timer` inside a module `utils`, which in turn is inside a module `my_project`.

Declarations outside a module are put into the implicit **root module**, which all first-level modules are also part of. The module `my_project` defined in the preceding paragraph would be part of the root module.

###### Example (Declaration)

```
module Name

func foo(): String = 'Foo'

module Name2 do
  func foo(): Int = 17
end
```

The module `Name2` is located inside the surrounding module `Name`. Without any `use` qualifiers, the functions can be accessed using `Name.foo` and `Name.Name2.foo`. Both `foo` multi-functions are completely distinct, as their full names are different. Hence we each have a multi-function that contains only one function instance.

The requirement for the `do` when declaring module `Name2` is purely due to parsing ambiguities that would arise otherwise. Without the `do`, the parser might have to read the whole file to find the `end` that closes the block module declaration which the parser would first assume to be a top module declaration. This will eventually be removed once we introduce significant indentation.

###### Example (Access)

```
lore.Enum.map([1, 2, 3], lore.Math.increment)      // --> [2, 3, 4]
lore.String.concat(['Hello', ', ', 'world', '!'])  // --> 'Hello, world!'
```



### Name Resolution

**Name resolution** is the technique with which simple names are resolved to absolute name paths. A simple name refers to the last segment of a name path, e.g. `baz` in `foo.bar.baz`.

##### Defaults

**By default**, module members have access to certain other module members by their simple name. A member in a module `foo.bar` has default access to the following module members by simple name (and in this order):

- Locally declared members in the **same** local module.
- Imports of the local module `foo.bar`.
- Locally declared members in the surrounding module `foo`.
- Imports of the local module `foo`.
- Locally declared members in the surrounding local root module. 
  - Note: If a module is nested twice, the inner parent has precedence over the outer parent, and so on. In this case, `foo` has precedence over the local root module.
- Imports of the local root module.
- Globally declared members of `foo.bar`.
- Globally declared members of the root module.

A **local module** in respect to e.g. a function declaration refers to the module declaration that's placed specifically around the function declaration in source code. Because modules may be comprised of many module declarations sharing the same name across multiple fragments, the term local module is a necessary contrast to global modules.

Note that, given these precedence rules, the following situation occurs: If we access `baz` in a local module `foo.bar`, but `foo.bar.baz` is declared in a non-local module, and the current fragment has an import `use bin.ban.baz`, the import is preferred, so `baz` refers to `bin.ban.baz`. This is necessary so that adding a name to a module doesn't break existing code in other fragments.

###### Example

```
use lore.Math

module foo do
  use bin.ban.baz
  
  module bar do
    func test(): Int = baz()
    func west(): Real = 3.9
  end
end

module foo.bar do
  func baz(): Int = 5
  func east(): Real = 1.6
end

module foo do 
  func doo(): Int = Math.min(22, 7)
end
```

In this example, `test` has access to `west` as it's locally declared, `baz` from `use bin.ban.baz`, `foo` as it's locally declared in the local root module, `Math` via the import at the root, `east` as a global module member, and module `lore` (for example) via the global root scope. `foo.bar.baz` is not accessible to `test`, because `use bin.ban.baz`  has precedence. For `test`'s frame of reference, the second `module foo.bar` declaration is NOT a local module. `foo.doo` is not accessible, even though `foo` is a parent module of `foo.bar`, as children don't automatically inherit the global members of their parent modules.

##### Use Declarations

The `use` declaration can be used to **introduce simple names** for other module members at their point of use. It is also called an **import**. It has three flavors:

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



### Companion Modules

A type and a module may share a name. A module that bears the same name as a type is called a **companion module**. This module is *expected*, by convention, to contain functions for working with the type. For example, the `Option` type has a companion module `Option` that contains functions for working with options, such as `Option.get`.

**Struct objects** may also have a companion module, but an object's properties and its companion module's bindings may not share a name.

###### Example

```
module animals

trait Animal

module Animal do
  func breed(mother: Animal, father: Animal): Option[Animal] = None
end
```



### Naming Conventions

Modules are essentially used in two ways, which determines their naming convention:

1. Modules that **contain functions and global variables** are usually named in upper camel case. Often they will be used explicitly, so that functions from multiple modules don't clash. A good example is the module `lore.Enum`. Its functions will often be used together with the module name, e.g. `Enum.map([1, 2, 3], x => x + 1)` and `Enum.flatten(lists)`. Sometimes these modules are also companion modules. 
   - An exception to this convention is when functions or global variables are intended to be used without the qualifying module name. In such cases the module is best understood as a namespace.
2. Modules that only contain types and other modules are also informally known as **namespaces**. They are written in snake case, such as the `lore` namespace defined by the Pyramid standard library.



### TODO: Visibility

*(Visibility is not yet implemented in the compiler and won't be for some time. It's a relatively tangential feature that isn't crucial for building Lore's compiler in Lore.)*

Module members have two modes of **visibility:**

- **Private** members may only be accessed from the same module.
- **Public** members may be accessed anywhere.

All module members are **public by default**. Private members are declared by suffixing their declaration keywords with a `p`: `traitp`, `funcp`, and so on. If a single function definition is private, all function definitions of the collective **multi-function** must be private as well. This has to be applied *explicitly* so that multi-functions cannot be unexpectedly private.

###### Example

```
module my_project

trait Foo

module Foo do
  structp Implementation extends Foo
    mut counter: Int
  end
  
  func fresh(): Foo = Implementation(0)
  
  func get_and_increment(Foo): Int
  func get_and_increment(foo: Implementation): Int = do
    let result = foo.counter
    increment(foo)
    result
  end
  
  actp increment(foo: Implementation)
    foo.counter += 1
  end
end
```



### TODOs

- **Use anywhere:** The `use` declaration should be usable anywhere (inside expressions as a top-level expression) for more fine-grained control of names.
  - To support this, we can implement imports in *local scopes* as well.
- **Aliases:** The `use` declaration should allow the programmer to rename a given symbol.
- **Complex imports:** Allow imports such as `use lore.[Enum._, Tuple]` and `use lore.[Enum.[flat_map, map], Tuple]`.
- **Absolute declarations:** With the syntax `func .lore.core.to_string` (and the equivalent for other top-level declarations), a user can declare a function with an absolute path, which would disregard its surrounding module declarations. This would allow us to override core functions without the need for specifying a new module. Especially so because currently, wanting to override core/Enum/... functions basically disallows using a top module declaration. For many modules, this either necessitates defining core/Enum/... functions in a separate file, or surrounding all definitions in the file with a module declaration, even the "main" definitions (such as `lore.List` definitions in `pyramid/list.lore`). The latter approach introduces needless indentation, while the former approach fragments a logical unit into multiple files.
