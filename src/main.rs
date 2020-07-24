#![feature(peekable_next_if)]
use anyhow::{Context, Result};
use argh::FromArgs;
use lexer::Lexer;
use parser::Parser;
use std::fs::File;
use std::io::{prelude::*, stdin, stdout, BufReader};
use std::path::Path;

mod ast;
mod environment;
mod interpreter;
mod lexer;
mod parser;
mod tokens;
#[derive(FromArgs, Debug)]
/// Interpreter for the lox programming language. Built with Rust.
struct Args {
    /// source file
    #[argh(positional)]
    source_file: Option<String>,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    println!("{:?}", args);

    // If a path was provided, we will process a file
    // otherwise, we will go to a prompt.
    if let Some(path) = args.source_file {
        // Try to open file with given path
        let path = Path::new(&path);
        // Open file
        let file = File::open(path).with_context(|| "Could not open source file.".to_string())?;
        // Process the file
        process_file(&file)?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn process_file(file: &File) -> Result<()> {
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    // Reading whole file to string...is bad...
    reader
        .read_to_string(&mut input)
        .with_context(|| "Could not read file.".to_string())?;
    run(&input)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    println!("Running prompt...");
    loop {
        print!("> ");
        stdout().flush()?;
        let mut input = String::new();
        stdin()
            .read_line(&mut input)
            .with_context(|| "Error reading input.".to_string())?;
        if input.trim().is_empty() {
            // TODO: Ask user if they really want to exit.
            println!("Exiting...");
            break;
        }
        // Execute line
        if let Err(error) = run(&input) {
            // Print error but continue session
            println!("Error: {}", error.to_string());
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<()> {
    let mut lexer = Lexer::new();
    lexer.scan_tokens(&mut source.chars().peekable())?;
    let tokens = lexer.get_tokens();
    let mut parser = Parser::new(tokens);
    // interpret(parser::parse(&mut parser)?)?;
    Ok(())
}
