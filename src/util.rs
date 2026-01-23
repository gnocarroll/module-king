// err => some char could not be converted to u8
pub fn string_to_ascii(s: String) -> Result<Vec<u8>, core::char::TryFromCharError> {
    let mut ascii = Vec::<u8>::new();
    
    for c in s.chars() {
        ascii.push(c.try_into()?);
    }

    Ok(ascii)
}