use std::path::PathBuf;
use structopt::StructOpt;
#[derive(StructOpt, Debug)]
#[structopt(name = "tox")]
pub struct Cli {
    /// The source code file to be ran
    pub source: Option<PathBuf>,
    /// The template file where the ast/ir will be outputted
    #[structopt(short, long, parse(from_os_str))]
    pub template: PathBuf,
    #[structopt(short, long, parse(from_os_str))]
    /// The output destination of the template
    pub grammar: PathBuf,
    /// Output the ast tokens to stdout or the file provided
    /// by output
    #[structopt(short, long)]
    pub syntax: bool,
}

pub enum Commands {
    GenAst,
}
