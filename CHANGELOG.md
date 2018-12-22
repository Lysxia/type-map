# 0.1.5.0

- Add role annotations for all the various type maps.
  Now the parameters have nominal roles.

  Previously, they had phantom roles, which broke uses of `unsafeCoerce`.

# 0.1.4.0

- Add `(<:)` to `Dynamic`. (deepfire)

# 0.1.3.0

- Add `update` to `Dynamic`. (deepfire)

# 0.1.2.0

- Update `Dynamic`
  + Export `empty`
  + Add `size`, `delete`, `union`, `difference`, `intersection`

# 0.1.1.0

- Compatible with GHC 8.2
