# Notes

- **Idea:** Rename multi-functions to multi-methods and functions to methods to signal that they implement dispatch in contrast to "simple" function that are treated as values?
- **Idea:** Multi-functions should only dispatch over the first parameter list.
- **Idea:** The dot notation for multi-functions should require the "dotted" value to correspond to the first parameter list. In particular, if we dispatch over two parameters `a` and `b`, dot notation needs to be invoked like `(a, b).collide()`.