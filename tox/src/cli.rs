use crate::db::{DatabaseImpl, Diagnostics};
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
}

pub enum Commands {
    Parse,
}

impl Cli {
    pub(crate) fn run(&self) -> io::Result<()> {
        let command = Commands::Parse;
        match command {
            Commands::Parse => self.parse(),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn parse(&self) -> io::Result<()> {
        let mut db = DatabaseImpl::default();

        for file in &self.source {
            let handle = db.load_file(file);

            let source_file = db.parse(handle);

            if let Some(ref output) = self.output {
                write!(&mut File::open(output)?, "{}", dump_debug(&source_file))?;
            }
        }

        db.emit()?;

        Ok(())
    }
}
