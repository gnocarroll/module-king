use std::{collections::HashMap};

#[derive(Debug)]
pub enum ToAsciiErr {
    NotAscii(char),
    FromChar(core::char::TryFromCharError),
}

// err => some char could not be converted to u8
pub fn string_to_ascii(s: String) -> Result<Vec<u8>, ToAsciiErr> {
    let mut ascii = Vec::<u8>::new();

    for c in s.chars() {
        if !c.is_ascii() {
            return Err(ToAsciiErr::NotAscii(c));
        }

        let next_u8 = match c.try_into() {
            Ok(value) => value,
            Err(e) => {
                return Err(ToAsciiErr::FromChar(e));
            }
        };

        ascii.push(next_u8);
    }

    Ok(ascii)
}

// err => some char could not be converted to u8
pub fn ascii_to_string(ascii: Vec<u8>) -> String {
    ascii.into_iter().map(|val| val as char).collect()
}

struct DirInfo {
    map: HashMap<String, FileVariant>,
    filenames: Vec<String>,
}

pub enum FileVariant {
    Regular,
    Directory(DirInfo),
}

fn print_file_variant(
    file_variant: &FileVariant,
    indent: usize,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    match file_variant {
        FileVariant::Regular => write!(f, ""),
        FileVariant::Directory(dir_info) => {
            let mut ret = Ok(());

            for (name, variant) in &dir_info.map {
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

impl FileVariant {
    pub fn treewalk<'a>(&'a self, remove_ext: &'a str) -> FileVariantTreewalk<'a> {
        FileVariantTreewalk::new(self, remove_ext)
    }
}

struct DirIter {
    pub filenames: Vec<String>,
    pub idx: usize,
}

impl DirIter {
    fn current_name(&self) -> Option<&String> {
        self.filenames.get(self.idx)
    }
}

pub struct FileVariantTreewalk<'a> {
    filevariant: &'a FileVariant,
    levels: Vec<DirIter>,
    is_done: bool,
    remove_ext: &'a str,
}

impl<'a> FileVariantTreewalk<'_> {
    fn new(filevariant: &'a FileVariant, remove_ext: &'a str) -> FileVariantTreewalk<'a> {
        FileVariantTreewalk {
            filevariant,
            levels: Vec::new(),
            is_done: false,
            remove_ext,
        }
    }

    fn extend_from_filevariant(&mut self) {
        let mut filevariant = self.filevariant;

        for level in &self.levels {
            let filename = match level.current_name() {
                Some(name) => name,
                None => {
                    break;
                }
            };

            match filevariant {
                FileVariant::Regular => {
                    break;
                }
                FileVariant::Directory(dir_info) => {
                    filevariant = &dir_info.map[filename];
                }
            }
        }

        loop {
            match filevariant {
                FileVariant::Regular => {
                    break;
                }
                FileVariant::Directory(dir_info) => {
                    if dir_info.map.len() == 0 {
                        break;
                    }

                    let filenames = dir_info.filenames.clone();

                    filevariant = &dir_info.map[&filenames[0]];

                    self.levels.push(DirIter { filenames, idx: 0 });
                }
            }
        }
    }

    fn levels_step(&mut self) {
        loop {
            let last_level = match self.levels.last_mut() {
                Some(last) => last,
                None => {
                    // no levels left, done
                    self.is_done = true;

                    return;
                }
            };

            last_level.idx += 1;

            // check if last level has been exhausted, if so pop it
            // then will try to advance on previous level if possible etc.

            if last_level.idx >= last_level.filenames.len() {
                self.levels.pop();
                continue;
            }

            // extend e.g. if new filevariant is directory then will open children

            self.extend_from_filevariant();

            break;
        }
    }

    fn init(&mut self) {
        self.extend_from_filevariant();
    }
}

impl Iterator for FileVariantTreewalk<'_> {
    // yields file paths separated into vectors
    // e.g. a/b/c -> [a, b, c]
    type Item = Vec<String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_done {
            return None;
        }
        if self.levels.len() == 0 {
            self.init();

            if self.levels.len() == 0 {
                self.is_done = true;
                return None;
            }
        }

        let mut ret_path = Vec::new();

        for level in &self.levels {
            ret_path.push(level.filenames[level.idx].clone());
        }

        self.levels_step();

        // if final filename ends with ext to remove (e.g. language ext),
        // then truncate to remove that extension

        if let Some(last) = ret_path.last_mut() {
            if filename_has_ext(&last, self.remove_ext) {
                let mut ext_len = self.remove_ext.len();

                match self.remove_ext.chars().next() {
                    Some('.') => (), // Ok
                    _ => {
                        // also remove period => add 1 to ext len
                        ext_len += 1;
                    }
                }

                last.truncate(last.len() - ext_len);
            }
        }

        Some(ret_path)
    }
}

pub fn filename_has_ext(filename: impl AsRef<str>, ext: impl AsRef<str>) -> bool {
    let filename = filename.as_ref();
    let ext = ext.as_ref();

    if !filename.ends_with(ext) {
        return false;
    }

    // if ext begins with '.' then return true (period was already included)
    // otherwise check if char before ext is '.'

    if let Some('.') = ext.chars().next() {
        return true;
    }

    filename[..filename.len() - ext.len()].ends_with('.')
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

        let (filename, metadata) = match (entry.file_name().into_string(), entry.metadata()) {
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
        } else if filename_has_ext(&filename, ext) {
            ret_map.insert(filename, FileVariant::Regular);
        }
    }

    let mut filenames: Vec<String> = ret_map.iter().map(|(filename, _)| filename.clone()).collect();

    // want sorted filenames so when treewalking it will be same order of files every time

    filenames.sort();

    if ret_map.len() > 0 {
        Ok(Some(FileVariant::Directory(DirInfo { map: ret_map, filenames })))
    } else {
        Ok(None)
    }
}
