use std::{collections::HashMap, fmt::write};

// err => some char could not be converted to u8
pub fn string_to_ascii(s: String) -> Result<Vec<u8>, core::char::TryFromCharError> {
    let mut ascii = Vec::<u8>::new();
    
    for c in s.chars() {
        ascii.push(c.try_into()?);
    }

    Ok(ascii)
}

// err => some char could not be converted to u8
pub fn ascii_to_string(ascii: Vec<u8>) -> String {
    ascii.into_iter().map(|val| val as char).collect()
}


pub enum FileVariant {
    Regular,
    Directory(HashMap<String, FileVariant>),
}

fn print_file_variant(file_variant: &FileVariant, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match file_variant {
        FileVariant::Regular => write!(f, ""),
        FileVariant::Directory(map) => {
            let mut ret = Ok(());
            
            for (name, variant) in map {
                for _ in 0..indent {
                    write!(f, " ");
                }

                writeln!(f, "{}", name);

                ret = print_file_variant(variant, indent + 4, f);
            }
            
            ret
        }
    }
}

impl std::fmt::Display for FileVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_file_variant(self, 0, f)
    }
}

pub fn listdir_with_ext(path: &str, ext: &str) -> Result<Option<FileVariant>, ()> {
    let mut ret_map = HashMap::new();
    
    let dir_iter = match std::fs::read_dir(path) {
        Ok(iter) => iter,
        Err(_) => {
            return Err(());
        }
    };

    for entry in dir_iter {
        let entry = match entry {
            Ok(entry) => entry,
            Err(_) => {
                return Err(());
            }
        };

        let (filename, metadata) = match (
            entry.file_name().into_string(),
            entry.metadata(),
        ) {
            (Ok(s), Ok(meta)) => (s, meta),
            _ => {
                return Err(());
            }
        };

        // ignore hidden files

        if filename.starts_with(".") {
            continue;
        }

        if metadata.is_dir() {
            let child_path = std::path::Path::new(path).join(&filename);

            let child_path_str = match child_path.as_os_str().to_str() {
                Some(s) => s,
                None => return Err(()),
            };

            if let Some(file_variant) = listdir_with_ext(child_path_str, ext)? {
                ret_map.insert(filename, file_variant);
            }
        } else if filename.ends_with(ext) {
            ret_map.insert(filename, FileVariant::Regular);
        }
    }

    if ret_map.len() > 0 {
        Ok(Some(FileVariant::Directory(ret_map)))
    } else {
        Ok(None)
    }
}