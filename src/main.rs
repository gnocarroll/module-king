use std::{path::Path, process::ExitCode};

use crate::{
    constants::{LANG_FILE_EXT, MODULE_FILENAME},
    parse::AST,
    tokens::Tokens,
    util::listdir_with_ext,
};

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

    // locate files with language extension

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

    for mut modulepath in dir_structure.treewalk(LANG_FILE_EXT) {
        if modulepath.len() == 0 {
            eprintln!("Found module path of length 0");
            return 1.into();
        }

        // determine filepath corresponding to current module

        let mut filepath_buf = Path::new(&wd).to_path_buf();

        for filename in &modulepath {
            filepath_buf = filepath_buf.join(filename);
        }

        filepath_buf.add_extension(LANG_FILE_EXT);

        // check if last filename indicates it is the code for a directory-based module
        // if so, remove last filename from the modulepath

        if modulepath
            .last()
            .expect("should have at least one name in path")
            == MODULE_FILENAME
        {
            modulepath.pop();
        }

        eprintln!("MODULE PATH: {}", modulepath.join("."));
        eprintln!("FILEPATH: {:?}", filepath_buf);

        let filepath = match filepath_buf.as_os_str().to_str() {
            Some(s) => s,
            None => {
                eprintln!("conversion from path buf to str failed");
                return 1.into();
            }
        };

        // modify AST using file

        if let Err(e) = process_file(&mut ast, filepath, modulepath) {
            eprintln!("Processing of file {} failed: {}", filepath, e,);
        }
    }

    // pass 2 for semantic analysis

    ast.repair();

    if ast.has_errors() {
        ast.display_all_errors();
    } else {
        run::run(&ast);
    }

    0.into()
}

fn process_file(ast: &mut AST, filepath: &str, modulepath: Vec<String>) -> Result<(), String> {
    eprintln!("PROCESSING FILE: {filepath}");

    let file_string = match std::fs::read_to_string(filepath) {
        Ok(s) => s,
        Err(_) => {
            return Err(format!("reading in file {filepath} failed"));
        }
    };

    let file_string = match crate::util::string_to_ascii(file_string) {
        Ok(s) => s,
        Err(crate::util::ToAsciiErr::NotAscii(c)) => {
            return Err(format!(
                "a char in {filepath} could not be converted to u8: {c}"
            ));
        }
        Err(crate::util::ToAsciiErr::FromChar(e)) => {
            return Err(format!(
                "a char in {filepath} could not be converted to u8: {e}"
            ));
        }
    };

    let tokens = match scan::tokenize(&file_string) {
        Ok(tokens) => tokens,
        Err(msg) => {
            return Err(format!("Tokenization of file {} failed: {}", filepath, msg));
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

    let tokens = Tokens::new(file_string, tokens);

    parse::parse_file(ast, tokens, modulepath);

    Ok(())
}
