mod cli;
mod db;
use crate::cli::Cli;
use structopt::StructOpt as _;
use tracing_subscriber::{prelude::*, Registry};
use tracing_tree::HierarchicalLayer;
pub type ParseResult<T> = Result<T, ()>;

fn main() -> std::io::Result<()> {
    // let subscriber = Registry::default().with(HierarchicalLayer::new(2));
    // tracing::subscriber::set_global_default(subscriber).unwrap();
    let opt = Cli::from_args();

    opt.run()?;

    Ok(())
}
