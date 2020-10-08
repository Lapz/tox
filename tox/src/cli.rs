use crate::db::{DatabaseImpl, Diagnostics};
use errors::{FileDatabase, WithError};
use parser::{dump_debug, ParseDatabase};
use semant::HirDatabase;
use std::fs::File;
use std::io::{self, Write};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "tox")]
pub struct Cli {
    /// The source code file to be ran
    pub source: Vec<PathBuf>,
    /// The file where the ast/ir will be outputted
    #[structopt(short, long, parse(from_os_str))]
    pub output: Option<PathBuf>,
    #[structopt(short, long)]
    pub lex: bool,

    #[structopt(short, long)]
    pub ast: bool,
}

impl Cli {
    pub fn run(self) -> io::Result<()> {
        let db = DatabaseImpl::default();
        // let mut errors = Vec::new();

        for path in self.source {
            let handle = db.intern_file(path);

            let WithError(tokens, _) = db.lex(handle);

            if self.lex {
                if let Some(ref output) = self.output {
                    write!(&mut File::open(output)?, "{:#?}", tokens)?;
                } else {
                    println!("{:#?}", tokens);
                }
            }

            let WithError(source_file, _) = db.parse(handle);

            if self.ast {
                if let Some(ref output) = self.output {
                    write!(&mut File::open(output)?, "{}", dump_debug(&source_file))?;
                } else {
                    println!("{}", dump_debug(&source_file));
                }
            }

            let WithError((), mut errors) = db.infer(handle);

            db.emit(&mut errors)?;
        }

        Ok(())
    }
}
