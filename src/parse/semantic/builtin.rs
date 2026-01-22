// Module which enumerates builtin functions and allows for retrieval of their names

#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Builtin {
    // create generic type
    Map,
    List,

    // functions to work with builtin containers e.g. List, Map, String
    GenericPush,
    GenericGet,
    GenericExists,
    GenericLen,

    // heap allocation/free
    Malloc,
    Mfree,

    // working directory
    GetWD,
    SetWD,

    // list files in directory and read from file to String
    DirList,
    FileRead,

    // do not set discriminants and keep this last and it will be correct
    BuiltinCount,
}

use Builtin::*;

static BUILTIN_COUNT: u8 = unsafe {
    std::mem::transmute::<Builtin, u8>(Builtin::BuiltinCount)
};

impl Builtin {
    // do not use From trait to ret Option
    pub fn from_u8(value: u8) -> Option<Builtin> {
        if value < BUILTIN_COUNT {
            Some(unsafe { std::mem::transmute::<u8, Builtin>(value) })
        } else {
            None
        }
    }

    pub fn get_builtin_iter() -> BuiltinIter {
        BuiltinIter::default()
    }

    pub fn get_builtin_name(self) -> &'static str {
        match self {
            Map => "Map",
            List => "List",

            GenericPush => "Generic_push",
            GenericGet => "Generic_get",
            GenericExists => "Generic_exists",
            GenericLen => "Generic_len",

            Malloc => "m_alloc",
            Mfree => "m_free",

            GetWD => "get_wd",
            SetWD => "set_wd",

            DirList => "dir_list",
            FileRead => "file_read",

            BuiltinCount => "",
        }
    }
}

#[derive(Clone)]
pub struct BuiltinIter {
    builtin_idx: u8,
}

impl Default for BuiltinIter {
    fn default() -> Self {
        BuiltinIter { builtin_idx: 0 }
    }
}

impl Iterator for BuiltinIter {
    type Item = Builtin;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Builtin::from_u8(self.builtin_idx);

        if ret.is_none() {
            return ret;
        }

        self.builtin_idx += 1;

        ret
    }
}
