use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
struct Cli {
    #[arg(value_name = "XML FILE")]
    file: PathBuf,

    /// Dump a debug view the resulting Document
    #[arg(short, long, default_value = "false")]
    debug: bool,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    if let Err(err) = xmlrus::Parser::parse(cli.file) {
        eprintln!("{err}");
        std::process::exit(1);
    }

    Ok(())
}
