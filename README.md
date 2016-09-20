Cartesian
=========
Functions and types for working with three-dimensional coordinate systems.

For now, all functions assume a coordinate system where right is +X, up is +Y and forwards is +Z, unless otherwise stated.

Contributors
------------
Jonatan H Sundqvist


TODO
----
- [x] Use typeclass for Vectors (would save a lot of boilerplate)
      -- [ ] Replace 'Vector' with more specific classes, depending on the use case
      -- [ ] Perhaps 'Vector' could be an alias for types that support all vector ops
- [x] Allow functions to operate on any Vector-like type (including eg. Complex) (cf. the `Vector`-typeclass)
- [ ] Consistent naming scheme (eg. use Vector(2D|3D) or just Vector for both types)

- [ ] Decide on a public API (right now, exports are a mess)