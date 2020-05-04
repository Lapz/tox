# Tox

![](https://github.com/lapz/tox-rewrite/workflows/CI/badge.svg)
[![](https://tokei.rs/b1/github/lapz/tox-rewrite?category=code)](https://github.com/lapz/tox-rewrite)
<br />
This is the new experimental version of Tox.I've been working on this slowly but I do have a working lexer plus parser and I've started on semantic analysis.

It's built using salsa and rowan and codespan. The design is inspired by rust-analyzer along with other projects.

## Screenshots

<img src='./assets/program.png' width=250>
<img src='./assets/errors.png' width=250>
<img src='./assets/bar.png' width=250>

## TODO

- [x] Error when type is defined multiple times
- [x] Add resolve imports to ctx
- [ ] Warn on unused pattern variable
- [ ] Infer types
- [ ] Resolve external imports

## Build

Generate the ast

```bash
cargo run -p=tools -- -s -g=syntax/src/grammer.ron -t=syntax/src/ast.rs.tera
```
