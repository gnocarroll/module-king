use std::collections::HashMap;

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

pub fn listdir_with_ext(path: &str, ext: &str) -> Result<FileVariant, ()> {
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

        if metadata.is_dir() {
            let child_path = std::path::Path::new(path).join(&filename);

            let child_path_str = match child_path.as_os_str().to_str() {
                Some(s) => s,
                None => return Err(()),
            };

            ret_map.insert(filename, listdir_with_ext(child_path_str, ext)?);
        } else if filename.ends_with(ext) {
            ret_map.insert(filename, FileVariant::Regular);
        }
    }

    Ok(FileVariant::Directory(ret_map))
}