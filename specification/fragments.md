# Fragments

**Fragments** are units of Lore source code, usually files. This documents describes or links to the various entities which may be declared at the top level of a fragment.

A fragment may contain the following **declarations:**

- [Functions and actions](multi-functions.md)
- [Trait and structs](traits-structs.md)
- [Type aliases](types.md#type-aliases)
- Domains

###### Example

```
func identity(x: A): A where A = x

act move(entity: Player, distance: Int) do
  move(entity.position, distance)
end

trait Monster extends Character

struct Zombie extends Monster
  mut health: Int
end

type StringFunction[A] = A => String

domain zombie: Zombie
  act damage(attack: Int) do
    zombie.health -= attack
  end
end
```



### Domains

A **domain** is a purely syntactical construct that surrounds function declarations. For each domain, one can specify a list of parameters and type parameters that get *prepended* to the parameter and type parameter lists of every function in the domain. Domains are **resolved** during parsing and have no bearing on run-time semantics or the execution environment. They cannot be nested.

Domains help to **avoid repetition** when defining multiple functions over the same partial set of parameters and type parameters. They are inspired by methods found in object-oriented languages, but obviously not constrained to such usage. While we don't want to tie functions to objects, like object-oriented programming does, we still want to provide some syntactic advantages that object-oriented programming offers. Like many other syntactic features in Lore, there is no obligation to use domains. It is simply a question of style.

###### Example 1

```
@where A
domain list: [A]
  func last(): A = get(list, length(list) - 1)
  func init(): [A] = slice(list, 0, length(list) - 1)
end
```

The above code will be effectively **translated** to the following code during parsing:

```
@where A
func last(list: [A]): A = get(list, length(list) - 1)

@where A
func init(list: [A]): [A] = slice(list, 0, length(list) - 1)
```

###### Example 2

Like functions, domains may use the inline and annotation styles of **where** clauses, but not both at the same time:

```
domain x: A where A
  identity(): A = x
  pair(): (A, A) = (x, x)
end

@where A, E
domain list: [A], element: E
  append(): [A | E] = list :+ element
  prepend(): [A | E] = concat([element], list)
end
```

