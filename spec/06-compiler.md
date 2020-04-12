# Compiler

[Previous File](05-expressions.md)

In this document, we outline the **design of the compiler**. It is prudent to consider the high-level design of the compiler on a conceptual level first, for three reasons: one, it will be easier for other developers to understand the structure of the compiler; two, while developing the compiler, we can always go back to these documents and periodically audit whether the implementation is still following a good direction; and three, I hope to make fewer mistakes coding the compiler when I have considered the high-level design first.



## Error Reporting

Undoubtedly, one of the most important aspects of a good compiler is the ability to provide clear **error handling**. In this sense, *the first version of the Lore compiler will be crap*. (I want to make it work before I "make it clear.")

Nonetheless, I want to offer some preliminary thoughts on error reporting. First of all, errors should be **collected** on a fragment-by-fragment or declaration-by-declaration basis (in later phases). The compiler should stop when a phase has found errors, but it shouldn't report one error and stop the whole compilation. Hence, we cannot work with expressions. Rather, errors should be **accumulated for each fragment**, which can then be reported when a phase ends.



## Phases

The **phases** of the compiler are "themed" steps that either transform one **representation** into another representation, or validate a representation. For example, the parsing phase transforms the source code into an AST.



#### Representation 1: Source Code

At the beginning of the compilation chain lies the **source code**. Lore is aiming to be **file-system agnostic**, that is, not that it works on any file system, but that it shouldn't matter whether the compiler is fed files or something else. The basic self-contained unit of source code is called **fragment**. A fragment is, often, a file, whose source is read by the parser, but this doesn't always have to be the case.

Fragments can sometimes be processed independent of each other, maybe even in **parallel**. The same applies to function declarations, type declarations, and so on. We will design the 

Hence, starting off, the **initial representation** of Lore is text in the form of valid, invalid, or partially valid source code.



#### Phase 1: Parser

Each fragment is **parsed** using fastparse into an AST representation. As of now, error reporting in this stage isn't good, as fastparse's error reporting is quite unwieldy out of the box. We will need to spend more time to make it usable.



#### Representation 2: ASTs

A set of **ASTs**, each bundled within a fragment data structure. Each node of the AST has **index** information which determines where the node *started* in the source code.