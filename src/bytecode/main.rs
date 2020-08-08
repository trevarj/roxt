#![feature(drain_filter)]
use argh::FromArgs;
use memory::Memory;
use std::fs::File;
use std::io::{prelude::*, stdin, stdout, BufReader};
use std::path::Path;
use vm::{InterpretError, VM};

pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod lexer;
pub mod memory;
pub mod object;
pub mod value;
pub mod vm;
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

    // get memory unit
    let mut mem = Memory::new();
    // start VM
    let mut vm = VM::new(&mut mem);

    // If a path was provided, we will process a file
    // otherwise, we will go to a prompt.
    if let Some(path) = args.source_file {
        // Try to open file with given path
        let path = Path::new(&path);
        // Open file
        let file = File::open(path)?;
        // Process the file
        process_file(&file, &mut vm)?;
    } else {
        repl(&mut vm)?;
    }

    Ok(())
}

fn process_file(file: &File, vm: &mut VM) -> Result<(), Box<dyn std::error::Error>> {
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    // Reading whole file to string...is bad...
    reader.read_to_string(&mut input)?;
    vm.interpret(&input)?;
    Ok(())
}

fn repl(vm: &mut vm::VM<'_>) -> Result<(), Box<dyn std::error::Error>> {
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
        if let Err(error) = vm.interpret(&input) {
            // Print error but continue session
            println!("Error: {}", error.to_string());
        }
    }

    Ok(())
}
