use std::{collections::HashMap, fs, io::Read, process::ExitCode};

use crate::{args::{ArgParser, ArgQuantity}, tokens::Tokens};

mod args;
mod constants;
mod parse;
mod run;
mod scan;
mod tokens;

fn main() -> ExitCode {
    let mut arg_parser = ArgParser::default();

    let arg_infos = [("input", ArgQuantity::Multiple), ("-o", ArgQuantity::One)];

    for (name, quantity) in arg_infos {
        match arg_parser.add_argument(name, quantity) {
            Ok(_) => {}
            Err(msg) => {
                eprintln!("problem while adding arg {name}: {msg}");
                return 1.into();
            }
        }
    }

    println!("ARGS");

    for arg in std::env::args() {
        println!("{arg}");
    }

    // skip executable name in args

    let args = match arg_parser.parse_args(std::env::args().skip(1)) {
        Ok(map) => map,
        Err(msg) => {
            eprintln!("Bad args provided: {msg}");
            return 1.into();
        }
    };

    println!("nargs: {}", args.len());

    for (arg, vec) in &args {
        print!("{arg}:");

        for ref item in vec {
            print!(" {item}");
        }

        println!();
    }

    // read file(s) into memory

    let mut file_strings: HashMap<String, String> = HashMap::new();

    if let Some(input_files) = args.get("input") {
        for filename in input_files {
            let file_string = match fs::read_to_string(filename) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Failed to read in file {filename}: {e}");
                    return 1.into();
                }
            };

            file_strings.insert(filename.to_string(), file_string);
        }
    } else {
        let mut stdin_string = String::new();

        if let Err(e) = std::io::stdin().read_to_string(&mut stdin_string) {
            eprintln!("Problem occurred while reading from STDIN: {e}");
            return 1.into();
        }

        file_strings.insert("STDIN".to_string(), stdin_string);
    }

    println!();

    for (filename, file_string) in file_strings {
        let tokens = match scan::tokenize(file_string.as_str()) {
            Ok(tokens) => tokens,
            Err(msg) => {
                eprintln!("Tokenization of file {} failed: {}", filename, msg);
                return 1.into();
            }
        };

        println!("FILE: {filename}");

        for (idx, tok) in tokens.iter().enumerate() {
            println!(
                "{}; Ln {}, Col {}; {} \"{}\"",
                idx,
                tok.line,
                tok.column,
                tok.ttype,
                tok.as_str(file_string.as_str()),
            );
        }

        let mut tokens = Tokens::new(file_string.as_str(), &tokens);

        let ast = parse::parse_file(filename.as_str(), &mut tokens);

        // if no errors run interpreter

        if !ast.has_errors() {
            run::run(&tokens, &ast);
        }
    }

    0.into()
}
