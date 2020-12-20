mod cli;
mod db;
use crate::cli::Cli;
use structopt::StructOpt as _;
use tracing_subscriber;
pub type ParseResult<T> = Result<T, ()>;

fn main() -> std::io::Result<()> {
    tracing_subscriber::fmt::init();
    let opt = Cli::from_args();

    opt.run()?;

    Ok(())
}
