# Opinions

This document lists a few strong opinions that underlie the Lore language. These are not necessarily central concepts, but certainly controversial.

- **Lore does not have a null.** You *have* to use `Option[A]` instead, *especially* when you're interfacing with foreign language code. That means you have to declare any value you receive from a foreign language as an `Option` if you have any reasonable expectation that the value will be null (in which case the null value will be converted to None). Receiving a null value from foreign code *without* declaring the value as an Option return in an immediate *runtime exception*.
  - **Possible Exception:** Local variables or maybe even record properties may be uninitialised, but they won't be usable (meaning runtime error) before they are initialised.
  - **TODO:** `Option` support should be as native as possible.