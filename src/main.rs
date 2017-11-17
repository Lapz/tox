#[macro_use]
extern crate nom;
extern crate nom_locate;

pub mod token;
pub mod lexer;

use lexer::int_test;

fn main() {
    println!("Hello, world!");
    int_test();
}
