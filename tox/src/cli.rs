use crate::db::{DatabaseImpl, Diagnostics};
use codegen::CodegenDatabase;
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

    #[structopt(short, long)]
    pub trace: bool,
}

impl Cli {
    pub fn run(self) -> io::Result<i32> {
        let db = DatabaseImpl::default();
        // let mut errors = Vec::new();

        let mut exit = 0;

        for path in self.source {
            let handle = db.intern_file(path);

            let WithError(tokens, mut errors) = db.lex(handle);

            if self.lex {
                if let Some(ref output) = self.output {
                    write!(&mut File::open(output)?, "{:#?}", tokens)?;
                } else {
                    println!("{:#?}", tokens);
                }
            }

            if !errors.is_empty() {
                db.emit(&mut errors)?;
                exit = -1;
                break;
            }

            let WithError(source_file, mut errors) = db.parse(handle);

            if self.ast {
                if let Some(ref output) = self.output {
                    write!(&mut File::open(output)?, "{}", dump_debug(&source_file))?;
                } else {
                    println!("{}", dump_debug(&source_file));
                }
            }

            if !errors.is_empty() {
                db.emit(&mut errors)?;
                exit = -1;
                break;
            }

            let WithError(_program, mut errors) = db.lower(handle);
            let WithError(_type_map, error) = db.infer(handle);

            errors.extend(error);

            db.emit(&mut errors)?;


            println!("{:#?}",_type_map)

            // Codegen here

            // let WithError(_, _) = db.compile_to_asm(handle);
        }

        Ok(exit)
    }
}
