#![crate_name = "test_runner"]
#![crate_type = "bin"]

extern crate ansi_term;
extern crate walkdir;

use walkdir::WalkDir;
use std::process::Command;
use std::fs::File;
use std::io::Read;
use ansi_term::Colour::{Green, Red};

fn main() {
    let mut pass = 0i32;
    let mut fail = 0i32;

    for entry in WalkDir::new("../tests") {

        let entry = entry.unwrap();
        
        if entry.path().is_dir() {
            continue;
        }

        let mut tox = Command::new("cargo");
        let entry = entry.unwrap();
        let mut expected = Vec::with_capacity(5);

        let mut source = String::new();

        let mut file = File::open(entry.path().to_str().unwrap()).expect("File not found");

        file.read_to_string(&mut source)
            .expect("something went wrong reading the file");

        let pattern = "// expect:";

        for line in source.lines() {
            if let Some((index, _)) = line.match_indices(&pattern).next() {
                let from = index + pattern.len();
                let expects = line[from..].to_string();
                expected.push(expects);
            }
        }

        tox.args(&["run", "--", entry.path().to_str().unwrap()]);

        let output = tox.output().expect("failed to execute process");

        let output = String::from_utf8_lossy(&output.stdout);

        for expects in expected {
            if output.contains(&expects) {
                pass += 1;
            } else {
                fail += 1;
            }
        }
    }

    println!(
        "Pass:{} Fail:{}",
        Green.bold().paint(pass.to_string()),
        Red.bold().paint(fail.to_string())
    );
}
