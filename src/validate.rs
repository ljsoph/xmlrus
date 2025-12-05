// NameStartChar ::=   ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D]
//                    | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
// NameChar      ::=   NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
// Name          ::=   NameStartChar (NameChar)*
// Names         ::=   Name (#x20 Name)*
// Nmtoken       ::=   (NameChar)+
// Nmtokens      ::=   Nmtoken (#x20 Nmtoken)*
pub fn is_valid_name(name: &str) -> bool {
    if name.len() <= 1 {
        return false;
    }

    let mut chars = name.chars();

    // The first character of a Name MUST be a NameStartChar, and any other characters MUST be NameChars;
    if let Some(first_char) = chars.next()
        && !is_name_start_char(first_char)
    {
        return false;
    }

    chars.all(is_name_char)
}

pub fn is_name_start_char(value: char) -> bool {
    let value = u32::from(value);

    if value <= 128 {
        #[allow(clippy::cast_possible_truncation)]
        return matches!(value as u8 , b':' | b'A'..=b'Z' | b'_' | b'a'..=b'z');
    }

    matches!(value,
        0xC0..=0xD6
        | 0xD8..=0xF6
        | 0xF8..=0x2FF
        | 0x370..=0x37D
        | 0x37F..=0x1FFF
        | 0x200C..=0x200D
        | 0x2070..=0x218F
        | 0x2C00..=0x2FEF
        | 0x3001..=0xD7FF
        | 0xF900..=0xFDCF
        | 0xFDF0..=0xFFFD
        | 0x10000..=0xEFFFF
    )
}

pub fn is_name_char(value: char) -> bool {
    if is_name_start_char(value) {
        return true;
    }

    let value = u32::from(value);

    if value <= 128 {
        #[allow(clippy::cast_possible_truncation)]
        return matches!(value as u8, b'0'..=b'9' | b'-' | b'.');
    }

    matches!(value,
        0xB7
        | 0x0300..=0x036F
        | 0x203F..=0x2040
    )
}

/// Validate a single `char` is in the XML Character Range
pub fn is_xml_char(value: char) -> bool {
    let value = u32::from(value);

    matches!(value,
        0x9   // '\t'
        | 0xA // '\n'
        | 0xD // '\r'
        // Any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
        | 0x20..=0xD7FF
        | 0xE000..=0xFFFD
        | 0x10000..=0x0010_FFFF
    )
}

/// Validate a String contains only characters in the XML Character Range
pub fn is_xml_chars(seq: &str) -> Result<(), (usize, char)> {
    for (index, value) in seq.char_indices() {
        if !is_xml_char(value) {
            return Err((index, value));
        }
    }

    Ok(())
}
