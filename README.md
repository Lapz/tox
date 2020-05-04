# Tox

cargo run -p=tools -- -s -g=syntax/src/grammer.ron -t=syntax/src/ast.rs.tera
This is the new experimental version of Tox.I've been working on this slowly but I do have a working lexer plus parser and I've started on semantic analysis.

It's built using salsa and rowan. The design is inspired by rust-analazyer along with other projects.

## TODO

[ ] Parsing
[ ] Fix ast reperesentation of record literal
[ ] Add a tratir for record literal fields
[ ] Add an immutable scoped map
[ ] Add import syntax
[ ] Resolve external imports
