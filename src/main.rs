use std::{collections::HashMap, fs, io::Read, process::ExitCode};

use crate::{
    args::{ArgParser, ArgQuantity},
    constants::LANG_FILE_EXT,
    parse::AST,
    tokens::Tokens,
    util::listdir_with_ext,
};

mod args;
mod constants;
mod parse;
mod run;
mod scan;
mod tokens;
mod util;

fn main() -> ExitCode {
    let wd = match std::env::current_dir() {
        Ok(s) => match s.as_os_str().to_str() {
            Some(as_str) => as_str.to_string(),
            None => {
                eprintln!("Could not get working directory from OS string to str");
                return 1.into();
            }
        },
        Err(_) => {
            eprintln!("Could not get wd");
            return 1.into();
        }
    };

    let dir_structure = match listdir_with_ext(wd.as_str(), LANG_FILE_EXT) {
        Ok(dir_structure) => dir_structure,
        Err(_) => {
            eprintln!("Could not get list files in wd");
            return 1.into();
        }
    };

    let dir_structure = match dir_structure {
        Some(dir_structure) => dir_structure,
        None => {
            eprintln!("No code files to run");
            return 1.into();
        }
    };

    eprintln!("Found module structure:");
    eprintln!("{}", dir_structure);

    let mut ast = AST::default();

    // remove lang file ext from module path

    for modulepath in dir_structure.treewalk(LANG_FILE_EXT) {
        eprintln!("MODULE PATH: {}", modulepath.join("."));
    }

    0.into()
}

enum FailVariant {
    Read,
    Scan,
}

struct ProcessFail {
    pub msg: String,
    pub variant: FailVariant,
}

fn process_file(ast: &mut AST, filepath: &str, modulepath: Vec<String>) -> Result<(), ProcessFail> {
    eprintln!("PROCESSING FILE: {filepath}");

    eprintln!("modulepath: {}", modulepath.join("."));

    // for now do not actually process

    return Ok(());

    let file_string = match std::fs::read_to_string(filepath) {
        Ok(s) => s,
        Err(_) => {
            return Err(ProcessFail {
                msg: format!("reading in file {filepath} failed"),
                variant: FailVariant::Read,
            });
        }
    };

    let file_string = match crate::util::string_to_ascii(file_string) {
        Ok(s) => s,
        Err(_) => {
            return Err(ProcessFail {
                msg: format!("a char in {filepath} could not be converted to u8"),
                variant: FailVariant::Read,
            });
        }
    };

    let tokens = match scan::tokenize(&file_string) {
        Ok(tokens) => tokens,
        Err(msg) => {
            return Err(ProcessFail {
                msg: format!("Tokenization of file {} failed: {}", filepath, msg),
                variant: FailVariant::Read,
            });
        }
    };

    // for (idx, tok) in tokens.iter().enumerate() {
    //     eprintln!(
    //         "{}; Ln {}, Col {}; {} \"{}\"",
    //         idx,
    //         tok.line,
    //         tok.column,
    //         tok.ttype,
    //         tok.as_str(&file_string),
    //     );
    // }

    let mut tokens = Tokens::new(&file_string, &tokens);

    parse::parse_file(ast, &mut tokens, modulepath);

    Ok(())
}
