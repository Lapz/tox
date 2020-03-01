mod cli;
mod db;

use crate::cli::Cli;
use structopt::StructOpt as _;

pub type ParseResult<T> = Result<T, ()>;

fn main() -> std::io::Result<()> {
    let opt = Cli::from_args();

    opt.drive()?;

    Ok(())
}
