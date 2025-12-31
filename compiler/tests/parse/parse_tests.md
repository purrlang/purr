# Parse tests

This folder contains test cases for the purr0 parser. Tests are small source
files accompanied by expected serialized ASTs (or golden outputs). Initial
focus: `struct`, `func`, and `T?` shorthand.

Example test cases:
- `func_return_user.pu` — function that returns a `User` struct
- `option_shorthand.pu` — uses `User?` and verifies the parser emits `option<User>` in type nodes

Test harness: TBD (can be a small script that runs the parser executable or
invokes a reference parser implemented in another language).
