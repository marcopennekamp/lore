# Identifiers

Lore has various **rules** surrounding type and variable names:

- **Letters** and **digits** are defined as follows:
  
  ```
  <letter> ::= "A".."Z" | "a".."z"
  <digit> ::= "0".."9"
  ```
- **Identifiers** may be a combination of letters, digits, underscores, question marks, and exclamation marks. Unless otherwise noted, an identifier is used for any kind of name, including names of struct types and constructors, global variables, functions, variables, and more. 
  
  ```
  <identifier> ::= (<letter> | "_") (<letter> | <digit> | "_" | "?" | "!")*
  ```
- **Type identifiers** may additionally contain a `+` sign at any position. They are used to name type aliases and traits.  *Struct names* use the standard `<identifier>` because a struct's name is also the name of its constructor, which may not contain a `+` sign. *Type variable names* use the standard `<identifier>` because `+` is already used to declare covariant type parameters.
  
  ```
  <type-identifier> ::= (<letter> | "_" | "+") (<letter> | <digit> | "_" | "+" | "?" | "!")*
  ```



### Keywords

An identifier may never be equal to the following **keywords**:

```
_ and case cond do else extends false fixed for func if intrinsic let module not object or proc return spec struct then trait true type use var where while yield
```
