String functions:
- empty?

List functions:
- frequencies (see Elixir's Enum.frequencies)
- group_by
- default/ifEmpty: Evaluates to the given list or, if it is empty, to a default list or element.
- distinct/unique elements
- transpose

Map functions:
- Merge two maps that contain lists such that the lists are merged when keys exist in both maps.
- Invert maps (`K -> V` to `V -> [K]`).

Shape functions:
- A type-safe merge function for shapes. (It probably needs compiler support.)
