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
    dbg!(&cli);

    let source = &std::fs::read_to_string(cli.file)?;
    let document = match xmlrus::Parser::parse(source) {
        Ok(document) => document,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    if cli.debug {
        println!("{document:#?}");
    }

    Ok(())
}
