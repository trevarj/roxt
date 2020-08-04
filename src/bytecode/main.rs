#![feature(refcell_take)]
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
    let vm = VM::new(&mut mem);

    // If a path was provided, we will process a file
    // otherwise, we will go to a prompt.
    if let Some(path) = args.source_file {
        // Try to open file with given path
        let path = Path::new(&path);
        // Open file
        let file = File::open(path)?;
        // Process the file
        process_file(&file, &vm)?;
    } else {
        repl(&vm)?;
    }

    Ok(())
}

fn process_file(file: &File, vm: &VM) -> Result<(), Box<dyn std::error::Error>> {
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    // Reading whole file to string...is bad...
    reader.read_to_string(&mut input)?;
    vm.interpret(&input)?;
    Ok(())
}

fn repl(vm: &VM) -> Result<(), Box<dyn std::error::Error>> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use chunk::{Chunk, OpCode};
    use vm::VM;

    #[test]
    fn test_vm() {
        let mut mem = Memory::new();
        let mut c = Chunk::new("test chunk".to_string());
        // a = -1.2
        let idx = c.add_constant(value::Value::Number(1.2));
        c.write(OpCode::OpConstant(idx as u16), 123);
        c.write(OpCode::OpNegate, 123);

        // b = 2
        let idx = c.add_constant(value::Value::Number(2.));
        c.write(OpCode::OpConstant(idx as u16), 123);

        // a * b
        c.write(OpCode::OpMultiply, 123);

        c.write(OpCode::OpReturn, 123);
        println!("{}", c);
        let mut vm = VM::new(&mut mem);
        let result = vm.run(&c);
    }
}
