#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate rand;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;

mod token;
mod lexer;
mod pos;
mod ast;
mod parser;
mod cli;
mod object;
mod interpreter;
mod inference;
mod types;
mod resolver;
mod symbol;
mod env;
mod pprint;
mod builtins;
use cli::{repl, run, Cli};
use structopt::StructOpt;

// use interpreter::Interpreter;

fn main() {
    let opts = Cli::from_args();

    if let Some(file) = opts.source {
        run(file, opts.ptokens, opts.pprint, opts.env, opts.past);
    } else {
        repl(opts.ptokens, opts.pprint)
    }
}
