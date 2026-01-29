use std::{collections::HashMap, fs, io::Read, process::ExitCode};

use crate::{
    args::{ArgParser, ArgQuantity}, constants::LANG_FILE_EXT, tokens::Tokens, util::listdir_with_ext
};

mod args;
mod constants;
mod parse;
mod run;
mod scan;
mod tokens;
mod util;

const MANIFEST_PATH: &str = env!("CARGO_MANIFEST_DIR");


fn main() -> ExitCode {
    if std::env::set_current_dir(MANIFEST_PATH).is_err() {
        eprintln!("Failed to set working directory to crate directory.");
        return 1.into();
    }

    let wd = match std::env::current_dir() {
        Ok(s) => match s.as_os_str().to_str() {
            Some(as_str) => as_str,
            None => {
                eprintln!("Could not get working directory from OS string to str");
                return 1.into();
            }
        }
        Err(_) => {
            eprintln!("Could not get wd");
            return 1.into();
        }
    };

    let dir_structure = match listdir_with_ext(wd, LANG_FILE_EXT) {
        Ok(dir_structure) => dir_structure,
        Err(_) => {
            eprintln!("Could not get list files in wd");
            return 1.into();
        }
    };

    println!("{}", dir_structure);

    // let tokens = match scan::tokenize(&file_string) {
    //     Ok(tokens) => tokens,
    //     Err(msg) => {
    //         eprintln!("Tokenization of file {} failed: {}", filename, msg);
    //         return 1.into();
    //     }
    // };

    // println!("FILE: {filename}");

    // for (idx, tok) in tokens.iter().enumerate() {
    //     println!(
    //         "{}; Ln {}, Col {}; {} \"{}\"",
    //         idx,
    //         tok.line,
    //         tok.column,
    //         tok.ttype,
    //         tok.as_str(&file_string),
    //     );
    // }

    // let mut tokens = Tokens::new(&file_string, &tokens);

    // let ast = parse::parse_file(filename.as_str(), &mut tokens);

    // // if no errors run interpreter

    // if !ast.has_errors() {
    //     run::run(&tokens, &ast);
    // }

    0.into()
}
