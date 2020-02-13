mod cli;
use cli::{Cli, Commands};
use structopt::StructOpt as _;

fn generate_syntax(
    template: &std::path::Path,
    grammar: &std::path::Path,
) -> Result<(), Box<dyn std::error::Error>> {
    teraron::generate(template, grammar, teraron::Mode::Overwrite)
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut opts = Cli::from_args();

    let command = if opts.syntax {
        Commands::GenAst
    } else {
        return Err("The only command available is to generate the ast".into());
    };

    let template = opts.template;

    let grammar = opts.grammar;

    match command {
        Commands::GenAst => generate_syntax(&template, &grammar)?,
    }

    Ok(())
}
