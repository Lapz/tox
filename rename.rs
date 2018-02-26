#!/usr/bin/env run-cargo-script
//! ```cargo
//! [dependencies]
//! walkdir = "2"
//! ```

extern crate walkdir;

use walkdir::WalkDir;
use std::fs;


fn main() {
    for entry in WalkDir::new("./tests") {
    
     let entry = entry.unwrap();

    if entry.path().is_dir() {
            continue;
    }


    let old_name = entry.path().to_str().unwrap();

    let new_name = entry.path().to_str().unwrap().replace(".lox",".tox");

    std::fs::rename(old_name, new_name);
    }
}
