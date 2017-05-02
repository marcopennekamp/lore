# Expressions


### Tuple Construction

For a tuple type `(A1, ..., An)`, a corresponding tuple can be constructed as follows:

    (e1, ..., en) // with e1: A1, ..., en: An


### Function Application

A function `f: A => B` can be applied in the following way:

    f(a) // with a: A

If `A` is a tuple type, the tuple may be constructed inside the application parentheses directly:

    f(e1, ..., en) // with A = (A1, ..., An) and e1: A1, ..., en: An



