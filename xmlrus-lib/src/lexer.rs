use std::{fmt::Debug, io::Read};

use crate::{
    error::{self, ParseResult},
    validate,
};

#[derive(Copy, Clone, PartialEq)]
pub enum TokenKind<'a> {
    AttributeValue(&'a str),
    CharData(&'a str),
    Comment(&'a str),
    DTDEnd,
    DTDStart,
    DoubleQuote,
    EndTag(&'a str),
    Eof,
    Equal,
    Name(&'a str),
    Placeholder(&'a str),
    ProcessingInstructionEnd,
    ProcessingInstructionStart,
    PubidLiteral(&'a str),
    SingleQuote,
    StartTagEmpty,
    StartTagEnd,
    StartTagStart(&'a str),
    SystemLiteral(&'a str),
    Whitespace(&'a str),
}

impl<'a> Debug for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AttributeValue(value) => write!(f, "AttributeValue(\"{value}\")"),
            Self::CharData(char_data) => write!(f, "CharData(\"{char_data}\")"),
            Self::Comment(comment) => write!(f, "Comment(\"{comment}\")"),
            Self::DTDEnd => write!(f, "DocTypeDeclEnd"),
            Self::DTDStart => write!(f, "DocTypeDeclStart"),
            Self::DoubleQuote => write!(f, "DoubleQuote"),
            Self::EndTag(qname) => write!(f, "EndTag(\"{qname}\")"),
            Self::Eof => write!(f, "Eof"),
            Self::Equal => write!(f, "Equal"),
            Self::Name(name) => write!(f, "Name(\"{name}\")"),
            Self::Placeholder(arg0) => f.debug_tuple("Placeholder").field(arg0).finish(),
            Self::ProcessingInstructionEnd => write!(f, "ProcessingInstructionEnd"),
            Self::ProcessingInstructionStart => write!(f, "ProcessingInstructionStart"),
            Self::PubidLiteral(pubid_literal) => write!(f, "PubidLiteral(\"{pubid_literal}\")"),
            Self::SingleQuote => write!(f, "SingleQuote"),
            Self::StartTagEmpty => write!(f, "StartTagEmpty"),
            Self::StartTagEnd => write!(f, "StartTagEnd"),
            Self::StartTagStart(qname) => write!(f, "StartTag(\"{qname}\")"),
            Self::SystemLiteral(system_literal) => write!(f, "SystemLiteral(\"{system_literal}\")"),
            Self::Whitespace(_) => write!(f, "Whitespace"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    offset: usize,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind<'a>, offset: usize) -> Self {
        Self { kind, offset }
    }
}

pub struct InputStream<'a> {
    source: &'a str,
    pos: usize,
    tokens: Vec<Token<'a>>,
}

impl<'a> InputStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            tokens: Vec::new(),
        }
    }
    fn advance(&mut self, amount: usize) {
        self.pos += amount;
    }

    fn push_token(&mut self, kind: TokenKind<'a>, offset: usize) {
        println!("TokenKind::{:?}", kind);
        self.tokens.push(Token::new(kind, offset));
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn slice_from(&self, start: usize) -> &'a str {
        &self.source[start..self.pos]
    }

    fn starts_with(&self, prefix: &str) -> bool {
        if self.pos >= self.source.len() {
            return false;
        }

        self.source[self.pos..self.source.len()].starts_with(prefix)
    }

    fn current_byte(&self) -> Option<u8> {
        if self.is_at_end() {
            return None;
        }

        Some(self.source.as_bytes()[self.pos])
    }

    fn consume_whitespace(&mut self) {
        let offset = self.pos;
        while matches!(self.current_byte(), Some(b' ' | b'\t' | b'\n' | b'\r')) {
            self.advance(1);
        }

        if offset != self.pos {
            self.push_token(TokenKind::Whitespace(self.slice_from(offset)), offset);
        }
    }
}

pub fn tokenize<'a>(mut stream: InputStream<'a>) -> ParseResult<Vec<Token<'a>>> {
    parse_document(&mut stream)?;
    stream.push_token(TokenKind::Eof, stream.pos);
    Ok(stream.tokens)
}

fn parse_name<'a>(stream: &mut InputStream<'a>) -> &'a str {
    let start = stream.pos;

    while let Some(current) = stream.current_byte() {
        if !validate::is_name_char_u8(current) {
            break;
        }
        stream.advance(1);
    }

    stream.slice_from(start)
}

/// Parses the source input, emitting a stream of tokens to build up the
/// resulting `Document`
///
/// [1] Document  ::=  prolog element Misc*
fn parse_document<'a>(stream: &mut InputStream<'a>) -> ParseResult<()> {
    parse_prolog(stream)?;

    // Parse any comments, PIs, or whitespace before the root element
    while let Some(b) = stream.current_byte() {
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => stream.consume_whitespace(),
            b'<' if stream.starts_with("<?") => {
                parse_processing_instruction(stream);
            }
            b'<' if stream.starts_with("<!--") => {
                parse_comment(stream);
            }
            // Start of the root node, break and parse outside of the loop.
            b'<' => break,
            _ => {
                // Lexical Error or break and catch with syntax pass?
                break;
            }
        }
    }

    stream.consume_whitespace();
    parse_element(stream);
    parse_misc(stream)?;

    Ok(())
}

/// [10]  AttValue  ::=  '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute_value<'a>(stream: &mut InputStream<'a>) {
    // We only enter this method if we encounter a single or double quote.
    let delimiter = stream.current_byte().expect("single or double quote");
    let kind = if delimiter == b'"' {
        TokenKind::DoubleQuote
    } else {
        TokenKind::SingleQuote
    };

    stream.push_token(kind, stream.pos);
    stream.advance(1);

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b) if b == delimiter => break,
            Some(_) => stream.advance(1),
        }
    }

    stream.push_token(TokenKind::AttributeValue(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

// [11] SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn parse_system_literal<'a>(stream: &mut InputStream<'a>) {
    let (delimiter, kind) = match stream.current_byte() {
        Some(sq @ b'\'') => (sq, TokenKind::SingleQuote),
        Some(dq @ b'"') => (dq, TokenKind::DoubleQuote),
        Some(_) | None => return,
    };

    stream.push_token(kind, stream.pos);
    stream.advance(1);

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b) if b == delimiter => break,
            Some(_) => stream.advance(1),
        }
    }

    // TODO: dereference to obtain input (this one is going low on the priority list lol)
    stream.push_token(TokenKind::SystemLiteral(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

// [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
// [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn parse_public_id_literal<'a>(stream: &mut InputStream<'a>) {
    let (delimiter, kind) = match stream.current_byte() {
        Some(sq @ b'\'') => (sq, TokenKind::SingleQuote),
        Some(dq @ b'"') => (dq, TokenKind::DoubleQuote),
        Some(_) | None => return,
    };

    stream.push_token(kind, stream.pos);
    stream.advance(1);

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b) if b == delimiter => break,
            Some(_) => stream.advance(1),
        }
    }
    stream.push_token(TokenKind::PubidLiteral(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

/// [14]  CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn parse_chardata<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => break,
            Some(b'<' | b'&') => break,
            Some(_) => stream.advance(1),
        }
    }

    let char_data = stream.slice_from(offset);
    stream.push_token(TokenKind::CharData(char_data), offset);
}

/// [15]  Comment  ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
fn parse_comment<'a>(stream: &mut InputStream<'a>) {
    stream.advance(4);

    let offset = stream.pos;

    loop {
        match stream.current_byte() {
            None => return,
            Some(b'-') if stream.starts_with("-->") => break,
            Some(_) => stream.advance(1),
        }
    }

    stream.push_token(TokenKind::Comment(stream.slice_from(offset)), offset);
    stream.advance(3);
}

// [16] PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
// [17] PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;

    stream.push_token(TokenKind::ProcessingInstructionStart, offset);
    stream.advance(2);

    let target = parse_name(stream);
    stream.push_token(TokenKind::Name(target), offset);
    stream.consume_whitespace();

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b'?') if stream.starts_with("?>") => break,
            Some(_) => stream.advance(1),
        }
    }

    stream.push_token(TokenKind::CharData(stream.slice_from(offset)), offset);
    stream.push_token(TokenKind::ProcessingInstructionEnd, stream.pos);
    stream.advance(2);
}

/// [22] prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut InputStream<'a>) -> ParseResult<()> {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if stream.starts_with("<?xml") {
        parse_xml_decl(stream);
    }

    parse_misc(stream)?;

    if stream.starts_with("<!DOCTYPE") {
        parse_doc_type_decl(stream);
        parse_misc(stream)?;
    }

    Ok(())
}

/// [23] XMLDecl       ::=   '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
/// [24] VersionInfo   ::=   S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
/// [25] Eq            ::=   S? '=' S?
/// [26] VersionNum    ::=   '1.' [0-9]+
/// [80] EncodingDecl  ::=   S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
/// [81] EncName       ::=   [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
/// [32] SDDecl        ::=   S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn parse_xml_decl<'a>(stream: &mut InputStream<'a>) {
    stream.advance(2);

    let mut offset = stream.pos;
    stream.push_token(TokenKind::ProcessingInstructionStart, offset);

    offset = stream.pos;
    let pi_target = parse_name(stream);
    stream.push_token(TokenKind::Name(pi_target), offset);

    loop {
        stream.consume_whitespace();
        match stream.current_byte() {
            None => break,
            Some(b'"' | b'\'') => {
                parse_attribute_value(stream);
            }
            Some(b'=') => {
                offset = stream.pos;
                stream.push_token(TokenKind::Equal, offset);
                stream.advance(1);
            }
            Some(b'?') if stream.starts_with("?>") => {
                stream.push_token(TokenKind::ProcessingInstructionEnd, offset);
                stream.advance(2);
                break;
            }
            Some(_) => {
                offset = stream.pos;
                let name = parse_name(stream);
                stream.push_token(TokenKind::Name(name), offset);
            }
        }
    }
}

/// [27] Misc  ::=  Comment | PI | S
fn parse_misc<'a>(stream: &mut InputStream<'a>) -> ParseResult<()> {
    loop {
        match stream.current_byte() {
            None => break,
            Some(b' ' | b'\t' | b'\n' | b'\r') => stream.consume_whitespace(),
            Some(b'<') if stream.starts_with("<?") => {
                parse_processing_instruction(stream);
            }
            Some(b'<') if stream.starts_with("<!--") => {
                parse_comment(stream);
            }
            Some(_) => break,
        }
    }

    Ok(())
}

/// [28]  doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
/// [28a] DeclSep     ::=   PEReference | S [WFC: PE Between Declarations]
/// [28b] intSubset   ::=   (markupdecl | DeclSep)*
/// [29]  markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn parse_doc_type_decl<'a>(stream: &mut InputStream<'a>) {
    stream.advance(9);
    stream.consume_whitespace();

    let offset = stream.pos;
    let name = parse_name(stream);
    stream.push_token(TokenKind::Name(name), offset);

    // optional
    parse_external_id(stream);

    // intSubset
    if stream.starts_with("[") {
        stream.consume_whitespace();
        loop {
            match stream.current_byte() {
                None => break,
                Some(b']') => break,
                Some(b'<') if stream.starts_with("<!ELEMENT") => parse_element_type_decl(stream),
                Some(b'<') if stream.starts_with("<!ENTITY") => parse_entity_decl(stream),
                Some(b'<') if stream.starts_with("<!ATTLIST") => parse_attlist_decl(stream),
                Some(b'<') if stream.starts_with("<!NOTATION") => parse_notation_decl(stream),
                Some(b'<') if stream.starts_with("<?") => parse_processing_instruction(stream),
                Some(b'<') if stream.starts_with("<!--") => parse_comment(stream),
                Some(_) => {
                    // Lexical Error or break and catch with syntax pass?
                    break;
                }
            }
        }
    }

    stream.advance(1);
    stream.consume_whitespace();
    if stream.starts_with(">") {
        stream.push_token(TokenKind::DTDEnd, stream.pos);
    }
}

/// [39] element  ::=  EmptyElemTag | STag content ETag
fn parse_element<'a>(stream: &mut InputStream<'a>) {
    parse_element_start(stream);
    parse_content(stream);
    parse_etag(stream);
}

/// [40] STag          ::= '<' QName (S Attribute)* S? '>'
/// [44] EmptyElemTag  ::= '<' QName (S Attribute)* S? '/>'
fn parse_element_start<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;
    stream.advance(1);
    stream.consume_whitespace();
    let name = parse_name(stream);
    stream.push_token(TokenKind::StartTagStart(name), offset);

    loop {
        stream.consume_whitespace();
        let offset = stream.pos;
        match stream.current_byte() {
            None => return,
            Some(b'>') => {
                stream.push_token(TokenKind::StartTagEnd, offset);
                stream.advance(1);
                break;
            }
            Some(b'/') if stream.starts_with("/>") => {
                stream.push_token(TokenKind::StartTagEmpty, offset);
                stream.advance(2);
                break;
            }
            Some(b'=') => {
                stream.push_token(TokenKind::Equal, offset);
                stream.advance(1);
            }
            Some(b'"' | b'\'') => {
                parse_attribute_value(stream);
            }
            Some(_) => {
                let name = parse_name(stream);
                stream.push_token(TokenKind::Name(name), offset);
            }
        }
    }
}

// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut InputStream<'a>) {
    loop {
        stream.consume_whitespace();
        match stream.current_byte() {
            None => break,
            Some(b'<') if stream.starts_with("</") => break,
            Some(b'<') if stream.starts_with("<?") => parse_processing_instruction(stream),
            Some(b'<') if stream.starts_with("<![CDATA[") => unimplemented!("CDATA"),
            Some(b'<') if stream.starts_with("<!--") => parse_comment(stream),
            Some(b'<') => parse_element(stream),
            Some(b'&') => unimplemented!("reference"),
            Some(_) => parse_chardata(stream),
        }
    }
}

/// [42]  ETag  ::=  '</' Name S? '>'
fn parse_etag<'a>(stream: &mut InputStream<'a>) {
    stream.advance(2);
    stream.consume_whitespace();

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b'>') => break,
            Some(_) => stream.advance(1),
        }
    }
    let qname = stream.slice_from(offset);
    stream.push_token(TokenKind::EndTag(qname), offset);
    stream.advance(1);
}

// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut InputStream<'a>) {
    stream.advance(9);
    stream.consume_whitespace();
    unimplemented!("element type decl")
}

// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut InputStream<'a>) {
    stream.advance(9);
    unimplemented!("attlist decl")
}

// [53] AttDef         ::=  S Name S AttType S DefaultDecl
// [54] AttType        ::=  StringType | TokenizedType | EnumeratedType
// [55] StringType     ::=  'CDATA'
// [56] TokenizedType  ::=  'ID'
//                     | 'IDREF'
//                     | 'IDREFS'
//                     | 'ENTITY'
//                     | 'ENTITIES'
//                     | 'NMTOKEN'
//                     | 'NMTOKENS'
fn parse_att_def<'a>(stream: &mut InputStream<'a>) {
    unimplemented!("att def")
}

// [70] EntityDecl  ::=  GEDecl | PEDecl
// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
// [72] PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn parse_entity_decl<'a>(stream: &mut InputStream<'a>) {
    unimplemented!("entity decl")
}

// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(stream: &mut InputStream<'a>) {
    stream.consume_whitespace();

    if stream.starts_with("SYSTEM") {
        stream.advance(6);
        stream.consume_whitespace();

        parse_system_literal(stream);
        // return Ok(Some((system_literal, None)));
    }

    if stream.starts_with("PUBLIC") {
        stream.advance(6);
        stream.consume_whitespace();

        parse_public_id_literal(stream);
        stream.consume_whitespace();
        parse_system_literal(stream);

        // return Ok(Some((system_literal, Some(pub_id_literal))));
    }
}

// [82] NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
// [83] PublicID      ::=  'PUBLIC' S PubidLiteral
//
// Validity constraint: Unique Notation Name
fn parse_notation_decl<'a>(stream: &mut InputStream<'a>) {
    unimplemented!("notation decl")
}
