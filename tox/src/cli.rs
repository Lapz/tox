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
    /// Output the ast tokens to stdout or the file provided
    /// by output
    #[structopt(short, long)]
    pub lex: bool,
    /// Output the ast to stdout or the file provided
    /// by output
    #[structopt(short, long)]
    pub parse: bool,
}

pub enum Commands {
    Parse,
    Lex,
}
