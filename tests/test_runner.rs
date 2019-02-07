#![crate_name = "test_runner"]
#![crate_type = "bin"]

extern crate ansi_term;
extern crate tempfile;
extern crate walkdir;

use ansi_term::Colour::{Green, Red, Yellow};

use std::fs::File;
use std::io::Read;
use std::process::Command;
use walkdir::WalkDir;

fn main() {
    let mut pass = 0i32;
    let mut fail = 0i32;
    let mut num = 0i32;
    let mut failed = Vec::new();

    'outer: for entry in WalkDir::new("../tests/pass") {

        let entry = entry.unwrap();

        if entry.path().is_dir() {
            continue;
        }

        num +=1;

        let mut undisclosedc = Command::new("cargo");

        let mut expected = Vec::new();

        let mut source = String::new();

        let mut file = File::open(entry.path().to_str().unwrap()).expect("File not found");

        file.read_to_string(&mut source)
            .expect("something went wrong reading the file");

        let expect_pattern = "// expect:";
        let skip_pattern = "//skip";

        for line in source.lines() {
            match line.match_indices(&skip_pattern).next() {
                Some((_, _)) => continue 'outer,
                None => (),
            }

            if let Some((index, _)) = line.match_indices(&expect_pattern).next() {
                let from = index + expect_pattern.len();
                let expects = line[from..].to_string();
                expected.push(expects);
            }
        }

        undisclosedc.args(&["run", entry.path().to_str().unwrap()]);

        let output = undisclosedc.output().expect("failed to execute process");

        let output = String::from_utf8_lossy(&output.stdout);

        let mut got = 0;

        for expects in expected.iter() {
            if output.contains(expects) {
                got += 1;
            }else {
                println!("{:?}",expects);
            }
        }

        if got == expected.len() {
            pass += 1
        } else {

            fail += 1;

            failed.push(
                ::std::fs::canonicalize(entry.path())
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
            );
        }
    }

    println!(
        "Pass:{} Fail:{}",
        Green.bold().paint(pass.to_string()),
        Red.bold().paint(fail.to_string())
    );

    

    if !failed.is_empty() {
        for test in failed {
            println!("Test {} failed ", Yellow.bold().paint(test));
        }
    }

    assert!(fail == 0);

    for entry in WalkDir::new("../tests/fail") {
        let mut undisclosedc = Command::new("cargo");
        let entry = entry.unwrap();

        if entry.path().is_dir() {
            continue;
        }

        let mut source = String::new();

        let mut file = File::open(entry.path().to_str().unwrap()).expect("File not found");

        file.read_to_string(&mut source)
            .expect("something went wrong reading the file");

        undisclosedc.args(&["run", entry.path().to_str().unwrap()]);

        let mut expected = Vec::new();

        let pattern = "//error:";

        for line in source.lines() {
            if let Some((index, _)) = line.match_indices(&pattern).next() {
                let from = index + pattern.len();
                let expects = line[from..].to_string();
                expected.push(expects);
            }
        }

        let output = undisclosedc.output().expect("failed to execute process");

        let output = String::from_utf8_lossy(&output.stdout);

        let mut got = 0;

        for expects in expected.iter() {
            if output.contains(expects) {
                got += 1;
            }
        }

        if got == expected.len() {
            fail += 1
        }
    }
}
