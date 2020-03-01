use crate::db::{DatabaseImpl, Diagnostics};
use errors::{Diagnostic, FileId};
use parser::{dump_debug, FilesExt, ParseDatabase};
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
}

pub enum Cli2 {
    Parse { sources: Vec<PathBuf> },
    Lex {},
}
pub enum Commands {
    Parse,
    Lex,
}

pub struct WithInfo {
    db: DatabaseImpl,
    errors: Vec<Diagnostic<FileId>>,
}

impl Cli {
    pub(crate) fn drive(&self) -> io::Result<()> {
        let command = if self.lex {
            Commands::Lex
        } else {
            Commands::Parse
        };
        match command {
            Commands::Parse => {
                self.lex().and_then(|context| self.parse(context))?;
            }
            Commands::Lex => {
                self.lex()?;
            }
        }

        Ok(())
    }

    pub(crate) fn lex(&self) -> io::Result<WithInfo> {
        let mut db = DatabaseImpl::default();
        let mut errors = Vec::new();

        for file in &self.source {
            let handle = db.load_file(file);

            let tokens = match db.lex(handle) {
                Ok(source_file) => source_file,
                Err(more_errors) => {
                    errors.extend(more_errors);
                    continue;
                }
            };

            if self.lex {
                if let Some(ref output) = self.output {
                    write!(&mut File::open(output)?, "{:#?}", tokens)?;
                } else {
                    println!("{:#?}", tokens);
                }
            }
        }

        db.emit(&mut errors)?;

        Ok(WithInfo { db, errors })
    }

    pub(crate) fn parse(&self, info: WithInfo) -> io::Result<WithInfo> {
        let mut db = info.db;
        let mut errors = info.errors;

        for file in &self.source {
            let handle = db.load_file(file);

            let source_file = match db.parse(handle) {
                Ok(source_file) => source_file,
                Err(more_errors) => {
                    errors.extend(more_errors);
                    continue;
                }
            };

            if let Some(ref output) = self.output {
                write!(&mut File::open(output)?, "{}", dump_debug(&source_file))?;
            }
        }

        db.emit(&mut errors)?;

        Ok(WithInfo { db, errors })
    }

    pub fn run(&self, info: WithInfo) -> io::Result<()> {
        let mut db = info.db;
        let mut errors = info.errors;

        for file in &self.source {
            let handle = db.load_file(file);

            let source_file = match db.parse(handle) {
                Ok(source_file) => source_file,
                Err(more_errors) => {
                    errors.extend(more_errors);
                    continue;
                }
            };

            if let Some(ref output) = self.output {
                write!(&mut File::open(output)?, "{}", dump_debug(&source_file))?;
            }
        }

        Ok(())
    }
}
