use std::{arch::x86_64::_MM_FROUND_CUR_DIRECTION, collections::HashMap, error::Error, str::FromStr, thread::current};


#[derive(PartialEq)]
pub enum ArgQuantity {
    Zero,
    One,
    Multiple,
}

struct ArgInfo {
    name: String,
    quantity: ArgQuantity
}

pub struct ArgParser {
    positional_args: Vec<ArgInfo>,
    kwargs: HashMap<String, ArgQuantity>,
}

impl Default for ArgParser {
    fn default() -> Self {
        ArgParser{
            positional_args: Vec::new(),
            kwargs: HashMap::new(),
        }
    }
}

fn push_to_vec(map: &mut HashMap<String, Vec<String>>, key: &String, value: String) {
    if let Some(vec) = map.get_mut(key) {
        vec.push(value);
    }
    else {
        let vec = vec![value];
        
        map.insert(key.to_string(), vec);
    }
}

impl ArgParser {
    // arg is positional unless name begins with one or more hyphens
    pub fn add_argument(&mut self, name: &str, quantity: ArgQuantity) -> Result<(), &'static str> {
        let mut name_len = 0;
        
        let mut counting_hyphens: bool = true;
        let mut hyphen_count = 0;
        
        for c in name.chars() {
            if c.is_whitespace() {
                return Err("no whitespace allowed in name");
            }

            name_len += 1;
            
            if counting_hyphens && c == '-' {
                hyphen_count += 1;
            }
            else {
                counting_hyphens = false;
            }
        }

        if hyphen_count == name_len {
            return Err("name must not be all hyphens")
        }
        
        let string_name: String = name.chars().skip(hyphen_count).collect();

        if hyphen_count > 0 { // kwarg
            self.kwargs.insert(string_name, quantity);
        }
        else { // positional arg
            self.positional_args.push(ArgInfo { name: string_name, quantity: quantity });
        }

        Ok(())
    }

    pub fn parse_args(
        &self,
        args: impl Iterator<Item=impl AsRef<str>>
    ) -> Result<HashMap<String, Vec<String>>, String> {
        let mut ret_map: HashMap<String, Vec<String>> = HashMap::new();


        let mut current_key : Option<String> = None;

        let mut arg_pos = 0;
        let positional_args_count = self.positional_args.len();

        for arg in args {
            let arg_string = arg.as_ref().to_string();

            let mut hyphen_count = 0;
            let mut equal_pos = 0;
            let string_len = arg_string.chars().count();

            for (idx, c) in arg_string.chars().enumerate() {
                if hyphen_count == 0 && c == '-' {
                    hyphen_count += 1;
                    continue;
                }
                if c == '=' {
                    equal_pos = idx;
                    break;
                }
            }

            if hyphen_count == 0 && equal_pos == 0 {
                if let Some(ref key) = current_key {
                    push_to_vec(&mut ret_map, key, arg_string.to_string());
                }
                else {
                    while arg_pos < positional_args_count && self.positional_args[arg_pos].quantity == ArgQuantity::Zero {
                        arg_pos += 1;
                    }

                    if arg_pos >= positional_args_count {
                        return Err(format!("found unexpected positional argument number {arg_pos}: {arg_string}"))
                    }

                    push_to_vec(
                        &mut ret_map,
                        &self.positional_args[arg_pos].name,
                        arg_string.to_string()
                    );

                    if self.positional_args[arg_pos].quantity == ArgQuantity::One {
                        arg_pos += 1;
                    }
                }
            }
            else {
                if let Some(ref key) = current_key {
                    return Err(format!{"keyword argument {key} had no value provided"});
                }

                let key_len = if equal_pos == 0 {
                    string_len - hyphen_count
                } else {
                    equal_pos - hyphen_count
                };

                let key: String = arg_string.chars().skip(hyphen_count).take(key_len).collect();

                if key.chars().any(|c| c.is_whitespace()) {
                    return Err(format!{"unexpected whitespace in key {key}"});
                }

                if !self.kwargs.contains_key(&key) {
                    return Err(format!{"unexpected key {key} (not added to parser object)"});
                }

                if equal_pos == 0 {
                    if self.kwargs[&key] == ArgQuantity::Zero {
                        push_to_vec(&mut ret_map, &key, "".to_string());
                    }
                    else {
                        current_key = Some(key);
                    }
                }
                else {
                    push_to_vec(
                        &mut ret_map,
                        &key,
                        arg_string.chars().skip(equal_pos + 1).collect()
                    );
                }
            }

            if let Some(ref key) = current_key{
                match self.kwargs.get(key) {
                    Some(ArgQuantity::Multiple) => {}
                    _ => {
                        current_key = None;
                    }
                }
            }
        }

        if let Some(ref key) = current_key {
            return Err(format!{"keyword argument {key} had no value provided"});
        }

        return Ok(ret_map)
    }
}