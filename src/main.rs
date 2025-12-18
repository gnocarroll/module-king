use std::process::ExitCode;

use crate::args::{ArgParser, ArgQuantity};

pub mod args;

fn main() -> ExitCode {
    let mut arg_parser = ArgParser::default();

    let arg_infos = [
        ("input", ArgQuantity::Multiple),
        ("-o", ArgQuantity::One),
    ];

    for (name, quantity) in arg_infos {
        match arg_parser.add_argument(name, quantity) {
            Ok(_) => {},
            Err(msg) => {
                eprintln!("problem while adding arg {name}: {msg}");
                return 1.into();
            }
        }
    }

    let args = match arg_parser.parse_args(std::env::args()) {
        Ok(map) => map,
        Err(msg) => {
            eprintln!("Bad args provided: {msg}");
            return 1.into();
        }
    };

    for (ref arg, ref vec) in args {
        print!("{arg}:");

        for ref item in vec {
            print!(" {item}");
        }

        println!();
    }

    0.into()
}
