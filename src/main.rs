mod parser;
mod ast;
mod type_checker;
mod codegen;
mod datapack;
mod error;

use clap::Parser;
use std::path::PathBuf;

/// DataRust Compiler - Compiles DataRust code to Minecraft Datapacks
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input DataRust source file (.dr)
    #[arg(short, long)]
    input: PathBuf,

    /// Output directory for the datapack
    #[arg(short, long, default_value = "output")]
    output: PathBuf,

    /// Treat warnings as errors
    #[arg(long)]
    warn_as_error: bool,

    /// Enable debug output
    #[arg(long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    if args.debug {
        println!("DataRust Compiler v0.1.0");
        println!("Input: {}", args.input.display());
        println!("Output: {}", args.output.display());
    }

    // Run the compilation pipeline
    match compile(&args) {
        Ok(()) => {
            println!("✓ Compilation successful!");
            println!("  Datapack generated at: {}", args.output.display());
        }
        Err(e) => {
            eprintln!("✗ Compilation failed:");
            eprintln!("  {}", e);
            std::process::exit(1);
        }
    }
}

fn compile(args: &Args) -> Result<(), error::CompileError> {
    // Step 1: Read source file
    let source = std::fs::read_to_string(&args.input)
        .map_err(|e| error::CompileError::IoError(e.to_string()))?;

    if args.debug {
        println!("\n[1/5] Reading source file...");
    }

    // Step 2: Parse source code to AST
    if args.debug {
        println!("[2/5] Parsing source code...");
    }
    let ast = parser::parse(&source)?;

    // Step 3: Type checking and validation
    if args.debug {
        println!("[3/5] Type checking...");
    }
    let typed_ast = type_checker::check(ast, &source, args.warn_as_error)?;

    // Step 4: Code generation
    if args.debug {
        println!("[4/5] Generating Minecraft commands...");
    }
    let functions = codegen::generate(&typed_ast)?;

    // Step 5: Write datapack files
    if args.debug {
        println!("[5/5] Writing datapack files...");
    }
    datapack::write_datapack(&args.output, functions)?;

    Ok(())
}
