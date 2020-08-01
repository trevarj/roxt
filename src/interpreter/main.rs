#![feature(peekable_next_if)]
use argh::FromArgs;
use interpreter::Interpreter;
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
mod resolver;
mod tokens;
#[derive(FromArgs, Debug)]
/// Interpreter for the lox programming language. Built with Rust.
struct Args {
    /// source file
    #[argh(positional)]
    source_file: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Args = argh::from_env();
    println!("{:?}", args);

    // If a path was provided, we will process a file
    // otherwise, we will go to a prompt.
    if let Some(path) = args.source_file {
        // Try to open file with given path
        let path = Path::new(&path);
        // Open file
        let file = File::open(path)?;
        // Process the file
        process_file(&file)?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn process_file(file: &File) -> Result<(), Box<dyn std::error::Error>> {
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    // Reading whole file to string...is bad...
    reader.read_to_string(&mut input)?;
    run(&input)?;
    Ok(())
}

fn run_prompt() -> Result<(), Box<dyn std::error::Error>> {
    println!("Running prompt...");
    loop {
        print!("> ");
        stdout().flush()?;
        let mut input = String::new();
        stdin().read_line(&mut input)?;
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

fn run(source: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut lexer = Lexer::new();
    lexer.scan_tokens(&mut source.chars().peekable())?;
    let tokens = lexer.get_tokens();
    let mut parser = Parser::new(tokens);
    let interpreter = Interpreter::new();
    interpreter.interpret(parser::parse(&mut parser)?)?;
    Ok(())
}
