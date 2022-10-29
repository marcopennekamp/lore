# Modules

**Modules** separate top-level definitions into logical units. Modules are *not* restricted to files or library bounds: you can declare a module anywhere, regardless of the source file's name. You can split a module across many files, and declare multiple modules in the same file. You can even extend a module first declared in a library (including Lore's standard library Pyramid) with additional specialized functions, types, global variables, and modules. Modules can be nested arbitrarily.

A module can be **declared** in two ways:

1. **At the top of a file**, a declaration `module Name` without module members will include all other declarations in the file in the module called `Name`. Each file can only have a single top module declaration.
2. With a **block scope**, a module can be declared at any declaration-level position in the file. All declarations inside the module's block will be put into the module.

A declaration inside a module is called a **module member**. Module members may be accessed with the member access notation: `Module.member`. Such an expression is also called a **module path**.

A module path can also be used as the name in a module declaration, which effectively creates a nested module. A module declaration `module my_project.utils.Timer` creates a module `Timer` inside a module `utils`, which in turn is inside a module `my_project`.

Declarations outside a module are put into the implicit **root module**, which all first-level modules are also part of. The module `my_project` defined above would be part of the root module.

###### Example (Declaration)

```
module Name

func foo(): String = 'Foo'

module Name2
  func foo(): Int = 17
```

The module `Name2` is located inside the surrounding module `Name`. Without any `use` qualifiers, the functions can be accessed using `Name.foo` and `Name.Name2.foo`. Both `foo` multi-functions are completely distinct, as their full names are different. Hence, we have two multi-functions that each contain only one function definition.

###### Example (Access)

```
lore.list.map([1, 2, 3], lore.number.inc)          --> [2, 3, 4]
lore.string.concat(['Hello', ', ', 'world', '!'])  --> 'Hello, world!'
```



### Name Resolution

**Name resolution** is the technique with which simple names are resolved to absolute name paths. A simple name refers to the last segment of a name path, e.g. `baz` in `foo.bar.baz`.

Module members have access to certain other module members by their simple name. In general, a member is accessible by simple name if it is declared/imported in the current or a parent local module, or if it is part of the current or a parent local module's global counterpart. Local declaration/imports take precedence over global members, and an inner module takes precedence over an outer module. 

A **local module** refers to the module declaration that's syntactically placed around a declaration in a fragment. Because modules may be comprised of many module declarations sharing the same name across multiple fragments, local modules are a necessary contrast to **global modules**. Each declaration is part of exactly one local module.

Internally, a non-root top module has a parent local root module regardless, with the top module placed inside the otherwise empty, implicit local root module. This ensures that name resolution always terminates at the root module and that non-root local modules always have a parent, which prevents the need to treat the root module as a special case during name resolution.

A local module that is declared with a contracted name such as `module foo.bar` does not automatically admit access to `foo`'s global members by simple name, only `bar`'s global members. Any local module gives simple-name access to its global members even to nested local modules, simply because it's unintuitive if a local module can't access all members that its parent local module can also access. Contracted modules don't have this issue, as there is no code that lives in the `foo` part.

Consider the following local module hierarchy:

```
module top

module foo.bar
  module baz
    -- ...
```

A member in the local module `baz` has access to the following module members by simple name (and in this precedence):

- Imports and locally declared members of `baz`.
- Imports and locally declared members of `foo.bar`.
- Imports and locally declared members of `top`.
- Globally declared members of `baz`.
- Globally declared members of `foo.bar`.
- Globally declared members of `top`.
- Globally declared members of the root module.

Note that, given these precedence rules, the following situation occurs: If we access `member` in a local module `foo.bar`, but `foo.bar.member` is declared in a non-local module, and the current fragment has an import `use a.b.member`, the import is preferred, so `member` refers to `a.b.member`. This is necessary so that adding a name to a module doesn't break existing code in other fragments.

###### Example

```
use lore.number

module foo
  use bin.ban.baz
  
  module bar
    func test(): Int = baz()
    func west(): Real = 3.9

module foo.bar
  func baz(): Int = 5
  func east(): Real = 1.6

module foo
  func doo(): Int = number.min(22, 7)
```

In this example, `test` has access to `west` as it's locally declared, `baz` from `use bin.ban.baz`, `foo` as it's locally declared in the local root module, `number` via the import at the root, `east` as a global module member, and module `lore` (for example) via the global root scope. `foo.bar.baz` is not accessible to `test`, because `use bin.ban.baz`  has precedence. For `test`'s frame of reference, the second `module foo.bar` declaration is NOT a local module. `foo.doo` is not accessible, even though `foo` is a parent module of `foo.bar`, as children don't automatically inherit the global members of their parent modules.

##### Imports

The `use` declaration can be used to introduce simple names for other module members at their point of use. It is also called an **import** and can only be placed at the beginning of a module declaration or at the beginning of a file. An import has three flavors:

1. **Single:** Import a single member directly.

   ```
   use lore.list.map
   ```

2. **List:** Import many members of the same module directly.

   ```
   use lore.list.[map, flat_map]
   ```

3. **Wildcard:** Import all members of a module.

   ```
   use lore.list._
   ```

Import paths must have at least two segments. The wildcard counts as a segment. Single imports such as `use A` are nonsensical and thus illegal. The first segment of an import path must refer to a module.

Direct imports have precedence over wildcard imports. That is, if a member is imported directly, an incompatible binding already imported by a wildcard import will be overridden. A later wildcard import can also override an earlier wildcard import of the same incompatible binding. An incompatible binding is either single-referable, or its kind doesn't agree with the new binding. Local declarations always take precedence over wildcard imports, starting from the first import.

Directly importing an incompatible module member whose simple name is already taken by another direct import or local declaration is illegal and will result in an error. In respect to local declarations, the restriction is in place to avoid an edge case: an import might shadow global bindings of the local module and bindings of the local module's parents, while the import itself might be useless when shadowed by a local declaration. Such an import confuses users and makes name resolution more complex to implement.

Imports may reference each other. For example, a wildcard import `use lore.option._` brings `lore.option.Option` into scope. Then, an import `use Option.some` directly imports `some`. Another wildcard import `use foo.bar._` might bring another `Option` into scope. Then, `Option` will refer to `foo.bar.Option`, while `some` still refers to `lore.option.Option.some`. The compiler applies the import precedence rules carefully and in order to achieve this. 

###### Example

```
-- Simple:
use lore.list.map
use lore.number.inc
[1, 2, 3] |> map(inc)

-- Multiple:
use lore.[Enum, number]
[1, 2, 3] |> Enum.map(number.inc)

-- Wildcard:
use lore.number._
use lore.math._
let x1 = (-b + sqrt(pow(b, 2) - product([4, a, c]))) / (2 * a)
```

###### Convention: Full names in `use`

Unless the import is targeting a companion module, it is good practice to use a fully qualified module name with `use`, even if it could be abbreviated. This makes an import such as `use lore.map.abc` possible while also defining a function `map` in the same scope. If the import was `use map.abc`, the compiler would try to import `abc` from the locally declared function `map`.

##### Name Resolution for Multi-Functions

**Multi-functions** have more involved name resolution semantics than other bindings. To allow importing multi-functions with the same simple names from different modules simultaneously, Lore supports compile-time disambiguation of multi-function calls and values. We also say that multi-functions are *multi-referable*.

When a simple-named multi-function is called or used as a value, the compiler simulates dispatch with all multi-function candidates that are accessible from the current scope, in two layers: local and global. These two layers are defined by the name resolution described above. The local layer contains all local definitions and imports, while the global layer contains global definitions that are not part of the local layer. The local layer has precedence.

The disambiguation is only successful if *one* dispatch attempt in the layer is successful and *all* other dispatch attempts fail. If none or multiple dispatch attempts are successful, the disambiguation has failed.

This process is separated into a local and a global layer so that new global declarations of multi-functions cannot conflict with imports or local definitions in existing code. If a multi-function `lore.list.map` is imported, and later a function `map` is defined globally and in scope, and if these two functions would conflict for a call `map([1, 2, 3], x => x * 2)`, `lore.list.map` will be preferred because it's imported locally. We could increase the number of layers to one per local or global module to achieve more granularity, but making the system more complex carries the possibility of confusing users and increases the burden on the compiler.

Keep in mind that the disambiguation happens at *compile time*. The specific multi-function will be chosen and compiled, unambiguously, into the `Call` or `Dispatch` bytecode instruction executed by the VM. The bytecode itself has no notion of simple names, scopes, or name resolution, and so at this point the choice will be final. This is similar to how some languages handle static method overloading.

###### Example

```
use lore.list.get!
use lore.option.[Some, get!]

struct Box(value: Int)

func get!(box: Box): Int = box.value

proc main() do
  get!([0, 1], 0)   -- calls lore.list.get!
  get!(Some(2))     -- calls lore.option.get!
  get!(Box(3))      -- calls the locally declared `get!`
```

Due to the type information available at compile time, it's easy for the compiler to disambiguate the various calls to `get!`. This is idiomatic for many domains, especially when the first argument is an "object" that a "method" is called on, reflecting object-oriented design. Note that such a first argument does not have to be a struct or trait; it can have any sort of type, such as `get!` for lists which expects a list as its first argument. The same applies to multi-functions with more than one argument, and not necessarily always the first argument.

Not all multi-functions should be imported and called like this, however. For example, a list function like `repeat`, which just takes any value as an argument, would be hard to disambiguate. Such functions are *not* tied to a domain and thus a little more fickle. `repeat` is best used as `List.repeat`.

###### Interaction with other bindings

Given the name resolution precedence, a non-multi-function definition or import shadows a multi-function from its outer local definitions and imports, and vice versa with multi-functions shadowing other bindings. Global definitions of inner modules are still taken into account, however. For example:

```
-- Multi-functions `nature.foo`, `nature.apple.foo`, `nature.bear.foo`, and `nature.cicada.foo` are defined in some
-- other fragment.
use nature.cicada.foo

module nature
  -- `nature.zebra.foo` is a global variable.
  use nature.zebra.foo

  module apple
    use nature.bear.foo

    -- `foo` refers to `nature.bear.foo` (local layer) AND `nature.apple.foo` (global layer), but NOT `nature.foo`
    -- because the local module `nature` imports `foo` as `nature.zebra.foo`. The import `nature.cicada.foo` has lower
    -- precedence than the import of `nature.zebra.foo` and must thus also be disregarded.
    let x: Int = foo()

  module bear
    -- `foo` refers to `zebra.foo`.
    let x: Int = foo
```

The import of the global variable `nature.zebra.foo` shadows the import of `nature.cicada.foo`, as well as the global definition of `nature.foo`. Hence, inside the local module `bear`, `foo` refers exclusively to the global variable.

The local module `apple` is where it gets tricky. The local import of `nature.bear.foo` reaffirms that `foo` should refer to a multi-function. The question is which local and global multi-functions are candidates for disambiguation:

- `nature.bear.foo` is trivially available as a local candidate, because it's imported directly.
- As `use nature.zebra.foo` shadows `use nature.cicada.foo` and the multi-function is not available as a global binding from any local modules, `nature.cicada.foo` is clearly not a candidate.
- Because `use nature.bear.foo` shadows `use nature.zebra.foo` and occurs inside the local module `apple`, `nature.apple.foo` should be available as a global candidate. The reasoning for this comes down to what the average programmer would expect inside the local module `apple`. The programmer is aware that `use nature.zebra.foo` has shadowed the import of `nature.cicada.foo`, so they don't expect `nature.cicada.foo` to be available anywhere inside the local module `nature`. The same applies to `nature.foo`, because usually the local module `nature` would make `nature.foo` available as a global binding, but the import overrides that. Hence, anywhere inside local module `apple`, because it's a child of local module `nature`, `nature.foo` is not available. However, `nature.apple.foo` *is* available because the local module `apple` makes it available as a global binding, similar to how `nature` would've made `nature.foo` available.
- Bonus question: Assume `foo` is a multi-function declared in the root scope. `foo` will also not be available in local module `apple`, because the root local module would usually make `foo` available, but this is overridden by the import of `nature.zebra.foo`. Hence, again, anywhere in local module `nature` where the import occurs, `foo` should not be available.

In summary, we have `nature.bear.foo` as a local candidate and `nature.apple.foo` as a global candidate. All results would be the same if `use nature.zebra.foo` was instead a local definition `let foo: Int = 5`.



### Companion Modules

A type and a module may share a name. A module that bears the same name as a type is called a **companion module**. This module usually contains definitions that will be used with the module name. For example, the `Option` type has a companion module `Option` that contains a secondary constructor `Option.some`.

**Struct objects** may also have a companion module, but an object's properties and its companion module's term bindings may not share a name.

###### Example

```
trait Option[+A]

module Option
  func some(value: A): Option[A] where A = Some(value)
```



### At-Root Module Declarations

Sometimes it is desirable to declare a nested module without it being a member of the outer module, but rather the root module. This is the case when overriding standard library functions such as `lore.core.to_string` and `lore.core.equal?`. Take the following incorrect code:

```
module culinary

struct Glass(content: String)

module lore.core
  func to_string(glass: Glass): String = 'A fine glass of ${glass.content}.'
```

The module `lore.core` will actually be `culinary.lore.core`. Even worse, if we're using an import such as `use lore.list.map`, the compiler will complain that `culinary.lore.list.map` does not exist! This is because we've declared a local module `lore` which takes precedence over the root `lore` module defined by the standard library.

To avoid this pitfall, an **at-root module declaration** ignores the outer module name and inserts the defined module directly into the root module:

```
module culinary

struct Glass(content: String)

@root
module lore.core
  func to_string(glass: Glass): String = 'A fine glass of ${glass.content}.'
```

As the at-root module is not a child of the outer module, its name will not become part of the local module. In the example above, a normal module declaration would be known as `lore` inside the local module `culinary`. However, because `lore` is an at-root module, `culinary` does not hold a local declaration `lore`. Rather, `lore` will be referred to as a globally declared member of the root module (see "Name Resolution" above).



### Naming Conventions

Depending on the purpose of a module, it might be named in snake case or upper camel case. The conventions are as follows:

1. Most modules should be named in snake case. These modules might contain types, and functions to work with these types. For example, the module `lore.option` contains the types `Option`, `Some`, and `None`, as well as functions that work on options, such as `get_or_else` and `map`. With compile-time disambiguation of multi-functions, the compiler is smart enough to associate the right `map` function with an `Option`, so there is no need to keep that function in an additional `Option` companion module. Modules that don't contain types are also usually named in snake case, as long as function names are specific enough. For example, the module `lore.sort` defines a function `merge_sort`, which is specific enough to be used without a qualifying upper-case `Sort` module.
2. Modules that are named in upper camel case contain types, global variables, and functions that are usually supposed to be used *with* the module name. For example, the function `lore.list.List.repeat` is meant to be used as `List.repeat`, because `repeat`'s arguments don't associate it with lists. The same applies to secondary constructors in companion modules. Other examples from Pyramid are `Option.some` and `Math.pi`. Lore doesn't stop you from importing these bindings directly, however. You can import and use `pi` directly at your discretion.



### TODO: Visibility

*(Visibility is not yet implemented in the compiler and won't be for some time. The feature's design is not finalized.)*

*TODO (syntax): Turn `private` into "local file and (same or parent) module" visibility. This will require attaching a qualifier name/UUID to private entities so that private declarations with the same full name don't conflict globally. The biggest issue with `private` as envisioned now is that the point of it is usually to hide implementation-specific functions. Those should NOT be visible in other files defining the same module. This is especially an issue for `lore.*` modules. `internal` could become the new "only visible in the same module" modifier.*

Module members have two modes of **visibility:**

- **Private** members may only be accessed from the same module.
- **Public** members may be accessed anywhere.

All module members are **public by default**. Private members are declared with the `private` modifier. If a single function definition is private, all function definitions of the collective **multi-function** must be private as well. This has to be applied *explicitly* so that multi-functions cannot be unexpectedly private.

###### Example

```
module my_project

trait Foo

module Foo
  private struct FooImpl extends Foo
    mut counter: Int
  
  func fresh(): Foo = FooImpl(0)
  
  func get_and_increment(Foo): Int
  
  -- TODO (syntax): This function exposes the type `FooImpl` and cannot be public. So it cannot work like that.
  func get_and_increment(foo: FooImpl): Int =
    let result = foo.counter
    increment(foo)
    result

  private proc increment(foo: FooImpl) do
    foo.counter += 1
```
