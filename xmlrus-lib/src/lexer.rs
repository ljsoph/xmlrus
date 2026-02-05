use std::fmt::Debug;

use crate::validate;

#[derive(Copy, Clone, PartialEq)]
pub enum TokenKind<'a> {
    /// <?xml
    XmlDeclStart,
    /// ?>
    XmlDeclEnd,
    /// `<!DOCTYPE`
    DTDStart,
    /// `>`
    DTDEnd,
    /// `[`
    IntSubsetStart,
    /// `]`
    IntSubsetEnd,

    // === markupdecl ===
    /// `<`
    MarkupDeclStart,
    /// `>`
    MarkupDeclEnd,
    /// `<![CDATA[`
    CDStart,
    /// `]]>`
    CDEnd,
    /// `<!ELEMENT`
    ElementDecl,
    /// `<!ENTITY`
    EntityDecl,
    /// `<!ATTLIST`
    AttlistDecl,
    /// `<!NOTATION`
    NotationDecl,
    /// Marks incoming EntityDef
    GEDecl,
    /// Marks incoming PEDef
    PEDecl,
    /// `NDATA`
    NData,
    /// `PUBLIC`
    Public,
    /// `SYSTEM`
    System,
    /// `"text between quotes"`
    Literal(&'a str),
    /// `([^%&"] | PEReference | Reference)*`
    EntityValue(&'a str),
    /// `&`
    EntityRef,
    /// `&#`
    CharRefDecimal,
    /// `&#x`
    CharRefHexadecimal,

    // === AttType ===
    /// `CDATA`
    CData,
    /// TokenizedType
    TokenizedType(TokenizedType),
    /// `NOTATION`
    NotationType,
    /// EnumeratedType
    Enumeration,
    /// `#REQUIRED`
    Required,
    /// `#IMPLIED`
    Implied,
    /// `#FIXED`
    Fixed,
    /// `EMPTY`
    Empty,
    /// `ANY`
    Any,
    /// `#PCDATA`
    PCData,

    // === Element ===
    /// `<`
    OpenTagStart,
    /// `</`
    TagEndStart,
    /// `/>`
    EmptyTagEnd,
    /// `>`
    TagEnd,
    /// Text between single or double quotes
    AttributeValue(&'a str),

    // === misc ===
    /// Text inside `content`, also used as the fallback when parsing unexpected characters
    CharData(&'a str),
    /// Text between `<!--` and `-->`
    Comment(&'a str),
    /// `Name` or `QName`
    Name(&'a str),
    /// NmToken
    NmToken(&'a str),
    /// `<?`
    PIStart,
    /// `?>`
    PIEnd,
    /// `=`
    Equal,
    /// `'`
    SingleQuote,
    /// `"`
    DoubleQuote,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `|`
    Pipe,
    /// `,`
    Comma,
    /// `?`
    QuestionMark,
    /// `*`
    Star,
    /// `+`
    Plus,
    /// `%`
    Percent,
    /// `;`
    SemiColon,
    /// space, tab, newline, or carriage return
    Whitespace(&'a str),
    /// `unreachable!()` without the panic
    Unreachable(&'static str),
    /// Error token to allow graceful error handling during parsing
    Error(&'static str),
    /// End of File
    Eof,
}

impl<'a> Debug for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::XmlDeclStart => write!(f, "XmlDeclStart"),
            Self::XmlDeclEnd => write!(f, "XmlDeclEnd"),
            Self::DTDEnd => write!(f, "DocTypeDeclEnd"),
            Self::DTDStart => write!(f, "DocTypeDeclStart"),
            Self::IntSubsetStart => write!(f, "IntSubsetStart"),
            Self::IntSubsetEnd => write!(f, "IntSubsetEnd"),
            Self::MarkupDeclStart => write!(f, "MarkupDeclStart"),
            Self::MarkupDeclEnd => write!(f, "MarkupDeclEnd"),
            Self::CDStart => write!(f, "CDStart"),
            Self::CDEnd => write!(f, "CDEnd"),
            Self::ElementDecl => write!(f, "ElementDecl"),
            Self::EntityDecl => write!(f, "EntityDecl"),
            Self::AttlistDecl => write!(f, "AttlistDecl"),
            Self::NotationDecl => write!(f, "NotationDecl"),
            Self::GEDecl => write!(f, "GEDecl"),
            Self::PEDecl => write!(f, "PEDecl"),
            Self::NData => write!(f, "NDATA"),
            Self::NotationType => write!(f, "NOTATION"),
            Self::Public => write!(f, "Public"),
            Self::System => write!(f, "System"),
            Self::Literal(literal) => write!(f, "Literal(\"{literal}\")"),
            Self::LeftParen => write!(f, "LeftParen"),
            Self::RightParen => write!(f, "RightParen"),
            Self::Pipe => write!(f, "Pipe"),
            Self::Comma => write!(f, "Comma"),
            Self::QuestionMark => write!(f, "QuestionMark"),
            Self::Star => write!(f, "Star"),
            Self::Plus => write!(f, "Plus"),
            Self::Required => write!(f, "Required"),
            Self::Implied => write!(f, "Implied"),
            Self::Fixed => write!(f, "Fixed"),
            Self::Empty => write!(f, "Empty"),
            Self::Any => write!(f, "Any"),
            Self::PCData => write!(f, "PCData"),
            Self::EntityValue(value) => write!(f, "EntityValue(\"{value}\")"),
            Self::EntityRef => write!(f, "EntityRef"),
            Self::CharRefDecimal => write!(f, "CharRefDecimal"),
            Self::CharRefHexadecimal => write!(f, "CharRefHexaecimal"),
            Self::CData => write!(f, "CData)"),
            Self::TokenizedType(tokenized_type) => write!(f, "TokenizedType(\"{tokenized_type:?}\")"),
            Self::Enumeration => write!(f, "Enumeration"),
            Self::OpenTagStart => write!(f, "OpenTagStart"),
            Self::TagEndStart => write!(f, "TagEndStart"),
            Self::EmptyTagEnd => write!(f, "EmptyTagEnd"),
            Self::TagEnd => write!(f, "TagEnd"),
            Self::AttributeValue(value) => write!(f, "AttributeValue(\"{value}\")"),
            Self::CharData(char_data) => write!(f, "CharData(\"{}\")", char_data.replace("\n", "")),
            Self::Percent => write!(f, "Percent"),
            Self::SemiColon => write!(f, "SemiColon"),
            Self::Comment(comment) => write!(f, "Comment(\"{comment}\")"),
            Self::Equal => write!(f, "Equal"),
            Self::Name(name) => write!(f, "Name(\"{name}\")"),
            Self::NmToken(nm_token) => write!(f, "NmToken(\"{nm_token}\")"),
            Self::PIStart => write!(f, "PIStart"),
            Self::PIEnd => write!(f, "PIEnd"),
            Self::SingleQuote => write!(f, "SingleQuote"),
            Self::DoubleQuote => write!(f, "DoubleQuote"),
            Self::Whitespace(_) => write!(f, "Whitespace"),
            Self::Unreachable(reason) => write!(f, "Unreachable({reason})"),
            Self::Error(reason) => write!(f, "Error({reason})"),
            Self::Eof => write!(f, "Eof"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenizedType {
    Id,
    IdRef,
    IdRefs,
    Entity,
    Entities,
    NmToken,
    NmTokens,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    offset: u32,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind<'a>, offset: usize) -> Self {
        Self {
            kind,
            offset: offset as u32,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum State {
    Default,
    XmlDecl,
    Comment,
    PI(bool),
    Dtd,
    IntSubset,
    ElementDecl,
    AttlistDecl,
    NotationDecl,
    DefaultDecl,
    EntityDecl,
    NDataDecl,
    NotationType,
    Enumeration,
    EntityDef,
    PEDef,
    ExternalId,
    ContentSpec,
    ElementStart,
    ElementEnd,
    Attributes,
    EntityRef,
    CharRef(bool),
    CData,
}

pub struct InputStream<'a> {
    source: &'a str,
    pos: usize,
    tokens: Vec<Token<'a>>,
    state: State,
    prev_state: State,
}

impl<'a> InputStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            tokens: Vec::new(),
            state: State::Default,
            prev_state: State::Default,
        }
    }

    fn set_state(&mut self, new_state: State) {
        println!("set_state: {:?} -> {:?}", self.state, new_state);
        self.prev_state = self.state;
        self.state = new_state;
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

    fn chomp(&mut self, kind: TokenKind<'a>, offset: usize) -> Option<Token<'a>> {
        println!("    TokenKind::{:?}", kind);
        Some(Token::new(kind, offset))
    }

    fn chomp_single(&mut self, kind: TokenKind<'a>) -> Option<Token<'a>> {
        self.advance(1);
        self.chomp(kind, self.pos - 1)
    }

    fn chomp_back(&mut self, kind: TokenKind<'a>, amount: usize) -> Option<Token<'a>> {
        self.advance(amount);
        self.chomp(kind, self.pos - amount)
    }

    fn chomp_ws(&mut self) -> Option<Token<'a>> {
        let offset = self.pos;

        while let Some(b) = self.current_byte() {
            if !self.is_ws(b) {
                break;
            }
            self.advance(1);
        }

        self.chomp(TokenKind::Whitespace(self.slice_from(offset)), offset)
    }

    fn is_ws(&self, b: u8) -> bool {
        matches!(b, 0x09 | 0x0A | 0x0D | 0x20)
    }

    fn chomp_name(&mut self) -> Option<&'a str> {
        let offset = self.pos;

        match self.current_byte() {
            None => return None,
            Some(b) if !validate::is_name_start_char_u8(b) => {
                // Option? Validate `NameStartChar` during parsing?
                self.advance(1);
                return Some("");
            }
            Some(_) => self.advance(1),
        }

        while let Some(b) = self.current_byte() {
            if !validate::is_name_char_u8(b) {
                break;
            }
            self.advance(1);
        }

        Some(self.slice_from(offset))
    }

    fn chomp_nm_token(&mut self) -> &'a str {
        let offset = self.pos;

        while let Some(b) = self.current_byte() {
            if !validate::is_name_char_u8(b) {
                break;
            }
            self.advance(1);
        }

        self.slice_from(offset)
    }

    fn chomp_until(&mut self, until: u8) -> &'a str {
        let offset = self.pos;
        while let Some(b) = self.current_byte()
            && b != until
        {
            self.advance(1);
        }

        self.slice_from(offset)
    }

    fn chomp_literal(&mut self, quote: u8) -> Option<Token<'a>> {
        let offset = self.pos;
        self.advance(1);
        let literal = self.chomp_until(quote);
        self.advance(1);
        self.chomp(TokenKind::Literal(literal), offset)
    }

    // 	CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
    fn chomp_char_data(&mut self) -> Option<&'a str> {
        let offset = self.pos;

        loop {
            match self.current_byte() {
                None => return None,
                Some(b'<' | b'&') => break,
                Some(b']') if self.starts_with("]]>") => break,
                Some(_) => self.advance(1),
            }
        }

        Some(self.slice_from(offset))
    }
}

impl<'a> Iterator for InputStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_byte() {
            None => None,
            Some(b) => match self.state {
                // 	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
                State::Default => match b {
                    b'<' if self.starts_with("<?xml") => {
                        self.set_state(State::XmlDecl);
                        self.chomp_back(TokenKind::XmlDeclStart, 2)
                    }
                    b'<' if self.starts_with("<?") => {
                        self.set_state(State::PI(false));
                        self.chomp_back(TokenKind::PIStart, 2)
                    }
                    b'<' if self.starts_with("<!--") => {
                        self.set_state(State::Comment);
                        self.next()
                    }
                    b'<' if self.starts_with("<!DOCTYPE") => {
                        self.set_state(State::Dtd);
                        self.chomp_back(TokenKind::DTDStart, 9)
                    }
                    b'<' if self.starts_with("<![CDATA[") => {
                        self.set_state(State::CData);
                        self.chomp_back(TokenKind::CDStart, 9)
                    }
                    b'<' if self.starts_with("</") => {
                        self.set_state(State::ElementEnd);
                        self.chomp_back(TokenKind::TagEndStart, 2)
                    }
                    b'<' => {
                        self.set_state(State::ElementStart);
                        self.chomp_single(TokenKind::OpenTagStart)
                    }
                    b'&' => {
                        if self.starts_with("&#x") {
                            self.set_state(State::CharRef(true));
                            self.chomp_back(TokenKind::CharRefHexadecimal, 3)
                        } else if self.starts_with("&#") {
                            self.set_state(State::CharRef(false));
                            self.chomp_back(TokenKind::CharRefDecimal, 2)
                        } else {
                            self.set_state(State::EntityRef);
                            self.chomp_single(TokenKind::EntityRef)
                        }
                    }
                    _ => {
                        let offset = self.pos;
                        let char_data = self.chomp_char_data()?;
                        self.chomp(TokenKind::CharData(char_data), offset)
                    }
                },
                State::XmlDecl => match b {
                    q @ b'\'' | q @ b'"' => self.chomp_literal(q),
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'=' => self.chomp_single(TokenKind::Equal),
                    b'?' if self.starts_with("?>") => {
                        self.set_state(State::Default);
                        self.chomp_back(TokenKind::XmlDeclEnd, 2)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::Comment => {
                    let offset = self.pos;
                    self.advance(4);

                    loop {
                        match self.current_byte() {
                            None => return None,
                            Some(b'-') if self.starts_with("-->") => break,
                            Some(_) => self.advance(1),
                        }
                    }

                    let comment = self.slice_from(offset);
                    self.advance(3);
                    self.set_state(self.prev_state);
                    self.chomp(TokenKind::Comment(comment), offset)
                }
                State::PI(target_chomped) => match b {
                    b'?' if self.starts_with("?>") => {
                        self.set_state(self.prev_state);
                        self.chomp_back(TokenKind::PIEnd, 2)
                    }
                    b if self.is_ws(b) => self.chomp_ws(),
                    _ => {
                        let offset = self.pos;
                        while let Some(b) = self.current_byte() {
                            match b {
                                b if self.starts_with("?>") || self.is_ws(b) => break,
                                _ => self.advance(1),
                            }
                        }

                        let value = self.slice_from(offset);
                        if target_chomped {
                            self.chomp(TokenKind::CharData(value), offset)
                        } else {
                            self.state = State::PI(true);
                            self.chomp(TokenKind::Name(value), offset)
                        }
                    }
                },
                State::Dtd => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'>' => {
                        self.set_state(State::Default);
                        self.chomp_single(TokenKind::DTDEnd)
                    }
                    b'[' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::IntSubsetStart)
                    }
                    b if self.starts_with("SYSTEM") || self.starts_with("PUBLIC") => {
                        self.set_state(State::ExternalId);
                        self.next()
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::ExternalId => match b {
                    q @ b'\'' | q @ b'"' => self.chomp_literal(q),
                    b if self.starts_with("SYSTEM") => self.chomp_back(TokenKind::System, 6),
                    b if self.starts_with("PUBLIC") => self.chomp_back(TokenKind::Public, 6),
                    b if self.is_ws(b) => self.chomp_ws(),
                    _ => {
                        self.set_state(self.prev_state);
                        self.next()
                    }
                },
                State::IntSubset => match b {
                    b'%' => self.chomp_single(TokenKind::Percent),
                    b';' => self.chomp_single(TokenKind::SemiColon),
                    b']' => {
                        self.set_state(State::Dtd);
                        self.chomp_single(TokenKind::IntSubsetEnd)
                    }
                    b if self.starts_with("<!ELEMENT") => {
                        self.set_state(State::ElementDecl);
                        self.chomp_back(TokenKind::ElementDecl, 9)
                    }
                    b if self.starts_with("<!ENTITY") => {
                        self.set_state(State::EntityDecl);
                        self.chomp_back(TokenKind::EntityDecl, 8)
                    }
                    b if self.starts_with("<!ATTLIST") => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_back(TokenKind::AttlistDecl, 9)
                    }
                    b if self.starts_with("<!NOTATION") => {
                        self.set_state(State::NotationDecl);
                        self.chomp_back(TokenKind::NotationDecl, 10)
                    }
                    b if self.starts_with("<?") => {
                        self.set_state(State::PI(false));
                        self.next()
                    }
                    b if self.starts_with("<!--") => {
                        self.set_state(State::Comment);
                        self.next()
                    }
                    b if self.is_ws(b) => self.chomp_ws(),
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::AttlistDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b if self.starts_with("CDATA") => self.chomp_back(TokenKind::CData, 5),
                    b if self.starts_with("IDREFS") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::IdRefs), 6)
                    }
                    b if self.starts_with("IDREF") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::IdRef), 5)
                    }
                    b if self.starts_with("ID") => self.chomp_back(TokenKind::TokenizedType(TokenizedType::Id), 2),
                    b if self.starts_with("ENTITY") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::Entity), 6)
                    }
                    b if self.starts_with("ENTITIES") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::Entities), 8)
                    }
                    b if self.starts_with("NMTOKENS") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::NmTokens), 8)
                    }
                    b if self.starts_with("NMTOKEN") => {
                        self.chomp_back(TokenKind::TokenizedType(TokenizedType::NmToken), 7)
                    }
                    b if self.starts_with("NOTATION") => {
                        self.set_state(State::NotationType);
                        self.chomp_back(TokenKind::NotationType, 8)
                    }
                    b'(' => {
                        self.set_state(State::Enumeration);
                        self.chomp(TokenKind::Enumeration, self.pos)
                    }
                    b'#' | b'"' => {
                        self.set_state(State::DefaultDecl);
                        self.next()
                    }
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::NotationType => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'(' => self.chomp_single(TokenKind::LeftParen),
                    b'|' => self.chomp_single(TokenKind::Pipe),
                    b')' => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_single(TokenKind::RightParen)
                    }
                    _ => {
                        // If a closing paren is never encountered, this will chomp empty names
                        // until one is encountered elsewhere or we reach the EOF. Should we revert
                        // to the previous state if we encounter an empty name?
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::Enumeration => match b {
                    b'(' => self.chomp_single(TokenKind::LeftParen),
                    b'|' => self.chomp_single(TokenKind::Pipe),
                    b')' => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_single(TokenKind::RightParen)
                    }
                    _ => {
                        let offset = self.pos;
                        let nm_token = self.chomp_nm_token();
                        self.chomp(TokenKind::NmToken(nm_token), offset)
                    }
                },
                State::DefaultDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    q @ b'\'' | q @ b'"' => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_literal(q)
                    }
                    b if self.starts_with("#REQUIRED") => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_back(TokenKind::Required, 9)
                    }
                    b if self.starts_with("#IMPLIED") => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_back(TokenKind::Implied, 8)
                    }
                    b if self.starts_with("#FIXED") => self.chomp_back(TokenKind::Fixed, 6),
                    _ => self.chomp_single(TokenKind::Unreachable("Invalid character in DefaultDecl")),
                },
                State::EntityDecl => match b {
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    b'%' => {
                        self.state = State::PEDef;
                        self.chomp_single(TokenKind::PEDecl)
                    }
                    b if self.is_ws(b) => self.chomp_ws(),
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.state = State::EntityDef;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::PEDef => match b {
                    q @ b'\'' | q @ b'"' => self.chomp_literal(q),
                    _ => {
                        self.state = State::ExternalId;
                        self.next()
                    }
                },
                State::EntityDef => match b {
                    q @ b'\'' | q @ b'"' => self.chomp_literal(q),
                    b if self.is_ws(b) => self.chomp_ws(),
                    b if self.starts_with("SYSTEM") || self.starts_with("PUBLIC") => {
                        self.set_state(State::ExternalId);
                        self.next()
                    }
                    b if self.starts_with("NDATA") => {
                        self.prev_state = State::EntityDecl;
                        self.state = State::NDataDecl;
                        self.chomp_back(TokenKind::NData, 5)
                    }
                    _ => {
                        self.set_state(State::EntityDecl);
                        self.next()
                    }
                },
                State::NDataDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.set_state(self.prev_state);
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::NotationDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    b if self.starts_with("SYSTEM") || self.starts_with("PUBLIC") => {
                        self.set_state(State::ExternalId);
                        self.next()
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::ElementDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b if self.starts_with("EMPTY") => self.chomp_back(TokenKind::Empty, 5),
                    b if self.starts_with("ANY") => self.chomp_back(TokenKind::Any, 3),
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    b'(' => {
                        self.set_state(State::ContentSpec);
                        self.chomp_single(TokenKind::LeftParen)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::ContentSpec => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'(' => self.chomp_single(TokenKind::LeftParen),
                    b')' => self.chomp_single(TokenKind::RightParen),
                    b'|' => self.chomp_single(TokenKind::Pipe),
                    b'?' => self.chomp_single(TokenKind::QuestionMark),
                    b'*' => self.chomp_single(TokenKind::Star),
                    b'+' => self.chomp_single(TokenKind::Plus),
                    b',' => self.chomp_single(TokenKind::Comma),
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    b if self.starts_with("#PCDATA") => self.chomp_back(TokenKind::PCData, 7),
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::ElementStart => {
                    let offset = self.pos;
                    let name = self.chomp_name()?;
                    self.set_state(State::Attributes);
                    self.chomp(TokenKind::Name(name), offset)
                }
                State::ElementEnd => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'>' => {
                        self.set_state(State::Default);
                        self.chomp_single(TokenKind::TagEnd)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::Attributes => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    q @ b'\'' | q @ b'"' => self.chomp_literal(q),
                    b'=' => self.chomp_single(TokenKind::Equal),
                    b'>' => {
                        self.set_state(State::Default);
                        self.chomp_single(TokenKind::TagEnd)
                    }
                    b'/' if self.starts_with("/>") => {
                        self.set_state(State::Default);
                        self.chomp_back(TokenKind::EmptyTagEnd, 2)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::CData => match b {
                    b']' if self.starts_with("]]>") => {
                        self.set_state(self.prev_state);
                        self.chomp_back(TokenKind::CDEnd, 3)
                    }
                    _ => {
                        let offset = self.pos;
                        let char_data = self.chomp_char_data()?;
                        self.chomp(TokenKind::CharData(char_data), offset)
                    }
                },
                State::EntityRef => match b {
                    b';' => {
                        self.set_state(self.prev_state);
                        self.chomp_single(TokenKind::SemiColon)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::CharRef(is_hex) => match b {
                    b';' => {
                        self.set_state(self.prev_state);
                        self.chomp_single(TokenKind::SemiColon)
                    }
                    _ => {
                        let offset = self.pos;
                        loop {
                            match (is_hex, self.current_byte()) {
                                (_, None) => return None,
                                (true, Some(b)) if !b.is_ascii_hexdigit() => break,
                                (false, Some(b)) if !b.is_ascii_digit() => break,
                                (_, Some(_)) => self.advance(1),
                            }
                        }

                        if offset == self.pos {
                            self.set_state(self.prev_state);
                            self.chomp(TokenKind::Error("Empty CharRef"), offset)
                        } else {
                            let char_ref = self.slice_from(offset);
                            self.chomp(TokenKind::Literal(char_ref), offset)
                        }
                    }
                },
            },
        }
    }
}

pub fn tokenize<'a>(mut stream: InputStream<'a>) -> Vec<Token<'a>> {
    chomp_document(&mut stream);
    stream.push_token(TokenKind::Eof, stream.pos);
    stream.tokens
}

fn chomp_whitespace<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;
    while matches!(stream.current_byte(), Some(b' ' | b'\t' | b'\n' | b'\r')) {
        stream.advance(1);
    }

    if offset != stream.pos {
        stream.push_token(TokenKind::Whitespace(stream.slice_from(offset)), offset);
    }
}

fn chomp_name<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;

    while let Some(current) = stream.current_byte() {
        if !validate::is_name_char_u8(current) {
            break;
        }
        stream.advance(1);
    }

    let name = stream.slice_from(offset);
    stream.push_token(TokenKind::Name(name), offset);
}

/// Parses the source input, emitting a stream of tokens to build up the
/// resulting `Document`
///
/// [1] Document  ::=  prolog element Misc*
fn chomp_document<'a>(stream: &mut InputStream<'a>) {
    chomp_prolog(stream);

    // Parse any comments, PIs, or whitespace before the root element
    while let Some(b) = stream.current_byte() {
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => chomp_whitespace(stream),
            b'<' if stream.starts_with("<?") => {
                chomp_processing_instruction(stream);
            }
            b'<' if stream.starts_with("<!--") => {
                chomp_comment(stream);
            }
            // Start of the root node, break and parse outside of the loop.
            b'<' => break,
            _ => {
                // This path is not considered well-formed, we will let the Parser handle any
                // errors originating from here.
                chomp_chardata(stream);
            }
        }
    }

    chomp_whitespace(stream);
    chomp_element(stream);
    chomp_misc(stream);
}

/// [9] EntityValue  ::=  '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
fn chomp_entity_value<'a>(stream: &mut InputStream<'a>) {
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

    stream.push_token(TokenKind::EntityValue(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

/// [10]  AttValue  ::=  '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
fn chomp_attribute_value<'a>(stream: &mut InputStream<'a>) {
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

/// [11] SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn chomp_system_literal<'a>(stream: &mut InputStream<'a>) {
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
    stream.push_token(TokenKind::Literal(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

/// [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
/// [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn chomp_public_id_literal<'a>(stream: &mut InputStream<'a>) {
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
    stream.push_token(TokenKind::Literal(stream.slice_from(offset)), offset);
    stream.push_token(kind, stream.pos);
    stream.advance(1);
}

/// [14]  CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn chomp_chardata<'a>(stream: &mut InputStream<'a>) {
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
fn chomp_comment<'a>(stream: &mut InputStream<'a>) {
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

/// [16] PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
/// [17] PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn chomp_processing_instruction<'a>(stream: &mut InputStream<'a>) {
    let offset = stream.pos;

    stream.push_token(TokenKind::PIStart, offset);
    stream.advance(2);

    chomp_name(stream);
    chomp_whitespace(stream);

    let offset = stream.pos;
    loop {
        match stream.current_byte() {
            None => return,
            Some(b'?') if stream.starts_with("?>") => break,
            Some(_) => stream.advance(1),
        }
    }

    stream.push_token(TokenKind::CharData(stream.slice_from(offset)), offset);
    stream.push_token(TokenKind::PIEnd, stream.pos);
    stream.advance(2);
}

/// [18] CDSect   ::=  CDStart CData CDEnd
/// [19] CDStart  ::=  '<![CDATA['
/// [20] CData    ::=  (Char* - (Char* ']]>' Char*))
/// [21] CDEnd    ::=  ']]>'
fn chomp_cdata<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::CDStart, stream.pos);
    stream.advance(9);

    loop {
        match stream.current_byte() {
            None => return,
            Some(b']') if stream.starts_with("]]>") => break,
            Some(_) => stream.advance(1),
        }
    }

    stream.push_token(TokenKind::CDEnd, stream.pos);
    stream.advance(3);
}

/// [22] prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn chomp_prolog<'a>(stream: &mut InputStream<'a>) {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if stream.starts_with("<?xml") {
        chomp_xml_decl(stream);
    }

    chomp_misc(stream);

    if stream.starts_with("<!DOCTYPE") {
        chomp_doc_type_decl(stream);
        chomp_misc(stream);
    }
}

/// [23] XMLDecl       ::=   '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
/// [24] VersionInfo   ::=   S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
/// [25] Eq            ::=   S? '=' S?
/// [26] VersionNum    ::=   '1.' [0-9]+
/// [80] EncodingDecl  ::=   S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
/// [81] EncName       ::=   [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
/// [32] SDDecl        ::=   S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn chomp_xml_decl<'a>(stream: &mut InputStream<'a>) {
    // TODO: Should this be its own token/token sequence to make parsing a bit easier?
    stream.advance(2);
    stream.push_token(TokenKind::PIStart, stream.pos);
    chomp_name(stream);

    loop {
        chomp_whitespace(stream);
        match stream.current_byte() {
            None => break,
            Some(b'"' | b'\'') => {
                chomp_attribute_value(stream);
            }
            Some(b'=') => {
                stream.push_token(TokenKind::Equal, stream.pos);
                stream.advance(1);
            }
            Some(b'?') if stream.starts_with("?>") => {
                stream.push_token(TokenKind::PIEnd, stream.pos);
                stream.advance(2);
                break;
            }
            Some(_) => {
                chomp_name(stream);
            }
        }
    }
}

/// [27] Misc  ::=  Comment | PI | S
fn chomp_misc<'a>(stream: &mut InputStream<'a>) {
    loop {
        chomp_whitespace(stream);
        match stream.current_byte() {
            Some(b'<') if stream.starts_with("<?") => {
                chomp_processing_instruction(stream);
            }
            Some(b'<') if stream.starts_with("<!--") => {
                chomp_comment(stream);
            }
            None | Some(_) => break,
        }
    }
}

/// [28]  doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
/// [28a] DeclSep     ::=   PEReference | S
/// [28b] intSubset   ::=   (markupdecl | DeclSep)*
/// [29]  markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn chomp_doc_type_decl<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::DTDStart, stream.pos);
    stream.advance(9);
    chomp_whitespace(stream);

    chomp_name(stream);

    // optional
    chomp_external_id(stream);

    // intSubset
    if stream.starts_with("[") {
        stream.push_token(TokenKind::IntSubsetStart, stream.pos);
        stream.advance(1);
        chomp_whitespace(stream);
        loop {
            match stream.current_byte() {
                None => return,
                Some(b'<') if stream.starts_with("<!ELEMENT") => chomp_element_type_decl(stream),
                Some(b'<') if stream.starts_with("<!ENTITY") => chomp_entity_decl(stream),
                Some(b'<') if stream.starts_with("<!ATTLIST") => chomp_attlist_decl(stream),
                Some(b'<') if stream.starts_with("<!NOTATION") => chomp_notation_decl(stream),
                Some(b'<') if stream.starts_with("<?") => chomp_processing_instruction(stream),
                Some(b'<') if stream.starts_with("<!--") => chomp_comment(stream),
                Some(b']') => {
                    stream.push_token(TokenKind::IntSubsetEnd, stream.pos);
                    stream.advance(1);
                    break;
                }
                Some(b'%') => {
                    stream.advance(1);
                    stream.push_token(TokenKind::Percent, stream.pos);
                    chomp_name(stream);
                }
                Some(b';') => {
                    stream.advance(1);
                    stream.push_token(TokenKind::SemiColon, stream.pos);
                }
                Some(b'>') => {
                    stream.push_token(TokenKind::MarkupDeclEnd, stream.pos);
                    stream.advance(1);
                    chomp_whitespace(stream);
                }
                Some(_) => {
                    chomp_chardata(stream);
                }
            }
        }
    }

    chomp_whitespace(stream);
    if stream.starts_with(">") {
        stream.advance(1);
        stream.push_token(TokenKind::DTDEnd, stream.pos);
    }
}

/// [39] element  ::=  EmptyElemTag | STag content ETag
fn chomp_element<'a>(stream: &mut InputStream<'a>) {
    if chomp_element_start(stream) {
        chomp_content(stream);
        chomp_etag(stream);
    }
}

/// [40] STag          ::= '<' QName (S Attribute)* S? '>'
/// [44] EmptyElemTag  ::= '<' QName (S Attribute)* S? '/>'
fn chomp_element_start<'a>(stream: &mut InputStream<'a>) -> bool {
    stream.push_token(TokenKind::OpenTagStart, stream.pos);
    stream.advance(1);
    chomp_whitespace(stream);
    chomp_name(stream);

    loop {
        chomp_whitespace(stream);
        let offset = stream.pos;
        match stream.current_byte() {
            None => break false,
            Some(b'>') => {
                stream.push_token(TokenKind::TagEnd, offset);
                stream.advance(1);
                break true;
            }
            Some(b'/') if stream.starts_with("/>") => {
                stream.push_token(TokenKind::EmptyTagEnd, offset);
                stream.advance(2);
                break false;
            }
            Some(b'=') => {
                stream.push_token(TokenKind::Equal, offset);
                stream.advance(1);
            }
            Some(b'"' | b'\'') => {
                chomp_attribute_value(stream);
            }
            Some(_) => {
                chomp_name(stream);
            }
        }
    }
}

/// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn chomp_content<'a>(stream: &mut InputStream<'a>) {
    loop {
        match stream.current_byte() {
            None => return,
            Some(b'<') if stream.starts_with("</") => break,
            Some(b'<') if stream.starts_with("<?") => chomp_processing_instruction(stream),
            Some(b'<') if stream.starts_with("<![CDATA[") => chomp_cdata(stream),
            Some(b'<') if stream.starts_with("<!--") => chomp_comment(stream),
            Some(b'<') => chomp_element(stream),
            Some(b'&') => unimplemented!("reference"),
            Some(_) => chomp_chardata(stream),
        }
    }
}

/// [42]  ETag  ::=  '</' Name S? '>'
fn chomp_etag<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::TagEndStart, stream.pos);
    stream.advance(2);
    chomp_whitespace(stream);
    chomp_name(stream);
    chomp_whitespace(stream);

    if let Some(b'>') = stream.current_byte() {
        stream.push_token(TokenKind::TagEnd, stream.pos);
        stream.advance(1);
    }
}

/// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
/// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn chomp_element_type_decl<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::MarkupDeclStart, stream.pos);
    stream.advance(9);
    chomp_whitespace(stream);
    unimplemented!("element type decl")
}

/// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn chomp_attlist_decl<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::MarkupDeclStart, stream.pos);
    stream.push_token(TokenKind::AttlistDecl, stream.pos);
    stream.advance(9);
    chomp_whitespace(stream);
    chomp_name(stream);
    chomp_att_def(stream);
    chomp_whitespace(stream);
}

/// [53] AttDef         ::=  S Name S AttType S DefaultDecl
fn chomp_att_def<'a>(stream: &mut InputStream<'a>) {
    chomp_whitespace(stream);
    chomp_name(stream);
    chomp_att_type(stream);
    chomp_whitespace(stream);
}

/// [54] AttType        ::=  StringType | TokenizedType | EnumeratedType
/// [55] StringType     ::=  'CDATA'
/// [56] TokenizedType  ::=  'ID'
///                     | 'IDREF'
///                     | 'IDREFS'
///                     | 'ENTITY'
///                     | 'ENTITIES'
///                     | 'NMTOKEN'
///                     | 'NMTOKENS'
fn chomp_att_type<'a>(stream: &mut InputStream<'a>) {
    if stream.starts_with("CDATA") {
        stream.push_token(TokenKind::CData, stream.pos);
        stream.advance(5);
    } else if stream.starts_with("ID") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::Id), stream.pos);
        stream.advance(2);
    } else if stream.starts_with("IDREF") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::IdRef), stream.pos);
        stream.advance(5);
    } else if stream.starts_with("IDREFS") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::IdRefs), stream.pos);
        stream.advance(6);
    } else if stream.starts_with("ENTITY") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::Entity), stream.pos);
        stream.advance(6);
    } else if stream.starts_with("ENTITIES") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::Entities), stream.pos);
        stream.advance(8);
    } else if stream.starts_with("NMTOKEN") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::NmToken), stream.pos);
        stream.advance(7);
    } else if stream.starts_with("NMTOKENS") {
        stream.push_token(TokenKind::TokenizedType(TokenizedType::NmTokens), stream.pos);
        stream.advance(8);
    } else {
        chomp_enumerated_type(stream)
    }
}

/// [57]  EnumeratedType  ::=  NotationType | Enumeration
fn chomp_enumerated_type<'a>(stream: &mut InputStream<'a>) {
    if stream.starts_with("NOTATION") {
        chomp_notation_type(stream);
    } else if stream.starts_with("(") {
        chomp_enumeration(stream);
    }
}

/// [58]  NotationType  ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
fn chomp_notation_type<'a>(stream: &mut InputStream<'a>) {
    unimplemented!("notation_typee")
}

/// [59]  Enumeration  ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
fn chomp_enumeration<'a>(stream: &mut InputStream<'a>) {
    unimplemented!("enumeratioin")
}

/// [70] EntityDecl  ::=  GEDecl | PEDecl
/// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn chomp_entity_decl<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::MarkupDeclStart, stream.pos);
    stream.push_token(TokenKind::EntityDecl, stream.pos);
    stream.advance(8);

    if let Some(b'%') = stream.current_byte() {
        stream.push_token(TokenKind::PEDecl, stream.pos);
        chomp_whitespace(stream);
        chomp_pe_def(stream);
        chomp_whitespace(stream);
    } else {
        stream.push_token(TokenKind::GEDecl, stream.pos);
        chomp_whitespace(stream);
        chomp_name(stream);
        chomp_whitespace(stream);
        chomp_entity_def(stream);
        chomp_whitespace(stream);
    }
}

/// [73] EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn chomp_entity_def<'a>(stream: &mut InputStream<'a>) {
    if let Some(b'\'' | b'"') = stream.current_byte() {
        chomp_entity_value(stream);
    } else {
        chomp_external_id(stream);
        chomp_ndata_decl(stream);
    }
}

/// [74]  PEDef  ::=  EntityValue | ExternalID
fn chomp_pe_def<'a>(stream: &mut InputStream<'a>) {
    stream.advance(1);

    chomp_whitespace(stream);
    chomp_name(stream);
    chomp_whitespace(stream);

    if let Some(b'\'' | b'"') = stream.current_byte() {
        chomp_entity_value(stream);
    } else {
        chomp_external_id(stream)
    }
}

/// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn chomp_external_id<'a>(stream: &mut InputStream<'a>) {
    chomp_whitespace(stream);

    if stream.starts_with("SYSTEM") {
        stream.advance(6);
        chomp_whitespace(stream);
        chomp_system_literal(stream);
    }

    if stream.starts_with("PUBLIC") {
        stream.advance(6);
        chomp_whitespace(stream);
        chomp_public_id_literal(stream);
        chomp_whitespace(stream);
        chomp_system_literal(stream);
    }
}

/// [76] NDataDecl  ::=  S 'NDATA' S Name
fn chomp_ndata_decl<'a>(stream: &mut InputStream<'a>) {
    chomp_whitespace(stream);

    if stream.starts_with("NDATA") {
        stream.push_token(TokenKind::NData, stream.pos);
        stream.advance(5);
        chomp_whitespace(stream);
        chomp_name(stream);
    }
}

/// [82] NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
/// [83] PublicID      ::=  'PUBLIC' S PubidLiteral
fn chomp_notation_decl<'a>(stream: &mut InputStream<'a>) {
    stream.push_token(TokenKind::MarkupDeclStart, stream.pos);
    unimplemented!("notation decl")
}

#[cfg(test)]
mod test {
    use super::TokenKind::NmToken as TKNmToken;
    use super::TokenKind::*;
    use super::TokenizedType::NmToken as TTNmToken;
    use super::TokenizedType::*;
    use super::*;

    macro_rules! next {
        ($stream:expr,$kind:expr) => {
            assert_eq!($kind, $stream.next().unwrap().kind);
        };
    }

    #[test]
    fn test_xml_decl() {
        let source = r#"<?xml version="1.0" encoding="UTF-8"?>"#;
        let mut stream = InputStream::new(&source);

        next!(stream, XmlDeclStart);
        next!(stream, Name("xml"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("version"));
        next!(stream, Equal);
        next!(stream, Literal("1.0"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("encoding"));
        next!(stream, Equal);
        next!(stream, Literal("UTF-8"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_empty() {
        let source = r#"<?xml?>"#;
        let mut stream = InputStream::new(&source);

        next!(stream, XmlDeclStart);
        next!(stream, Name("xml"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_empty_whitespace() {
        let source = "<?xml\t\t\n?>";
        let mut stream = InputStream::new(&source);

        next!(stream, XmlDeclStart);
        next!(stream, Name("xml"));
        next!(stream, Whitespace("\t\t\n"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_random_things() {
        let source = r#"<?xml foo bar baz?>"#;
        let mut stream = InputStream::new(&source);

        next!(stream, XmlDeclStart);
        next!(stream, Name("xml"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("bar"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("baz"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_entity_decl_simple() {
        let source = r#"<!ENTITY name "some name">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("some name"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_simple() {
        let source = r#"<!ENTITY name "some name">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("some name"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_simple_extra_whitespace() {
        let source = r#"<!ENTITY name "some name"   >"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("some name"));
        next!(stream, Whitespace("   "));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_system() {
        let source = r#"<!ENTITY name SYSTEM "foo">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("foo"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_public() {
        let source = r#"<!ENTITY name PUBLIC "foo" SYSTEM "bar">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_multiple_system() {
        let source = r#"<!ENTITY name SYSTEM "foo" SYSTEM "bar" SYSTEM>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_ndata() {
        let source = r#"<!ENTITY name SYSTEM "foo" NDATA gif>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, NData);
        next!(stream, Whitespace(" "));
        next!(stream, Name("gif"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_backwards() {
        let source = r#"<!ENTITY name SYSTEM "foo" PUBLIC "bar">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_externalid_no_literal() {
        let source = r#"<!ENTITY name SYSTEMSYSTEM SYSTEM>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_string_type() {
        let source = r#"<!ATTLIST two chapter CDATA #REQUIRED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("two"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("chapter"));
        next!(stream, Whitespace(" "));
        next!(stream, CData);
        next!(stream, Whitespace(" "));
        next!(stream, Required);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_id() {
        let source = r#"<!ATTLIST a attr ID #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(Id));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_idref() {
        let source = r#"<!ATTLIST a attr IDREF #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(IdRef));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_idrefs() {
        let source = r#"<!ATTLIST a attr IDREFS #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(IdRefs));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_entity() {
        let source = r#"<!ATTLIST a attr ENTITY #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(Entity));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_entities() {
        let source = r#"<!ATTLIST a attr ENTITIES #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(Entities));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_nmtoken() {
        let source = r#"<!ATTLIST a attr NMTOKEN #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(TTNmToken));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_nmtokens() {
        let source = r#"<!ATTLIST a attr NMTOKENS #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("attr"));
        next!(stream, Whitespace(" "));
        next!(stream, TokenizedType(NmTokens));
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_notation() {
        let source = r#"<!ATTLIST b att NOTATION (a|b) #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("att"));
        next!(stream, Whitespace(" "));
        next!(stream, NotationType);
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, Name("a"));
        next!(stream, Pipe);
        next!(stream, Name("b"));
        next!(stream, RightParen);
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_notation_ws() {
        let source = r#"<!ATTLIST b att NOTATION (  a | b    ) #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("att"));
        next!(stream, Whitespace(" "));
        next!(stream, NotationType);
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, Whitespace("  "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Pipe);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace("    "));
        next!(stream, RightParen);
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_notation_missing_parens() {
        let source = r#"<!ATTLIST b att NOTATION #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("att"));
        next!(stream, Whitespace(" "));
        next!(stream, NotationType);
        next!(stream, Whitespace(" "));
        next!(stream, Name(""));
        next!(stream, Name("IMPLIED"));
        next!(stream, Name(""));
        // EOF
    }

    #[test]
    fn test_attlist_decl_notation_empty_parens() {
        let source = r#"<!ATTLIST b att NOTATION () #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("att"));
        next!(stream, Whitespace(" "));
        next!(stream, NotationType);
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, RightParen);
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_attlist_decl_enumuration() {
        let source = r#"<!ATTLIST b att (a|b) #IMPLIED>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, AttlistDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("b"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("att"));
        next!(stream, Whitespace(" "));
        next!(stream, Enumeration);
        next!(stream, LeftParen);
        next!(stream, TKNmToken("a"));
        next!(stream, Pipe);
        next!(stream, TKNmToken("b"));
        next!(stream, RightParen);
        next!(stream, Whitespace(" "));
        next!(stream, Implied);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_decl_external_id_system() {
        let source = r#"<!NOTATION name SYSTEM "name.txt">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("name.txt"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_decl_external_id_public() {
        let source = r#"<!NOTATION JPGformat PUBLIC "jpg 1" "jpg 2">"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("JPGformat"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("jpg 1"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("jpg 2"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_decl_public_id() {
        let source = r#"<!NOTATION a PUBLIC "b"  >"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("a"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("b"));
        next!(stream, Whitespace("  "));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_empty() {
        let source = r#"<!ELEMENT name EMPTY>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Empty);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_any() {
        let source = r#"<!ELEMENT name ANY>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Any);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_any_bad_casing() {
        let source = r#"<!ELEMENT name any>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("any"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_mixed() {
        let source = r#"<!ELEMENT name (#PCDATA)>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, PCData);
        next!(stream, RightParen);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_mixed_names() {
        let source = r#"<!ELEMENT name (#PCDATA|a|b|c)>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, PCData);
        next!(stream, Pipe);
        next!(stream, Name("a"));
        next!(stream, Pipe);
        next!(stream, Name("b"));
        next!(stream, Pipe);
        next!(stream, Name("c"));
        next!(stream, RightParen);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_mixed_no_paren() {
        let source = r#"<!ELEMENT name #PCDATA>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Name(""));
        next!(stream, Name("PCDATA"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_children() {
        let source = r#"<!ELEMENT div1 (head, (p | list | note)*, div2*)>"#;
        let mut stream = InputStream::new(&source);
        stream.state = State::IntSubset;

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("div1"));
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, Name("head"));
        next!(stream, Comma);
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, Name("p"));
        next!(stream, Whitespace(" "));
        next!(stream, Pipe);
        next!(stream, Whitespace(" "));
        next!(stream, Name("list"));
        next!(stream, Whitespace(" "));
        next!(stream, Pipe);
        next!(stream, Whitespace(" "));
        next!(stream, Name("note"));
        next!(stream, RightParen);
        next!(stream, Star);
        next!(stream, Comma);
        next!(stream, Whitespace(" "));
        next!(stream, Name("div2"));
        next!(stream, Star);
        next!(stream, RightParen);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_entity_ref() {
        let source = r#"&da_ref;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, EntityRef);
        next!(stream, Name("da_ref"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_entity_ref_double_ampersand() {
        let source = r#"&&da_ref;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, EntityRef);
        next!(stream, Name(""));
        next!(stream, Name("da_ref"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_entity_ref_multiple_semi() {
        let source = r#"&da_ref;;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, EntityRef);
        next!(stream, Name("da_ref"));
        next!(stream, SemiColon);
        assert!(stream.next().is_none()) // EOF reached while parsing `CharData`
    }

    #[test]
    fn test_char_ref_decimal() {
        let source = r#"&#123;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, CharRefDecimal);
        next!(stream, Literal("123"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_char_ref_decimal_invalid_digit() {
        let source = r#"&#123f;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, CharRefDecimal);
        next!(stream, Literal("123"));
        next!(stream, Error("Empty CharRef"));
        assert!(stream.next().is_none()) // EOF reached while parsing `CharData`
    }

    #[test]
    fn test_char_ref_decimal_invalid_digit_correction() {
        let source = r#"<a>&#123f;</a>"#;
        let mut stream = InputStream::new(&source);

        next!(stream, OpenTagStart);
        next!(stream, Name("a"));
        next!(stream, TagEnd);
        next!(stream, CharRefDecimal);
        next!(stream, Literal("123"));
        next!(stream, Error("Empty CharRef"));
        next!(stream, CharData("f;"));
        next!(stream, TagEndStart);
        next!(stream, Name("a"));
        next!(stream, TagEnd);
    }

    #[test]
    fn test_char_ref_hexadecimal() {
        let source = r#"&#xff1233;"#;
        let mut stream = InputStream::new(&source);

        next!(stream, CharRefHexadecimal);
        next!(stream, Literal("ff1233"));
        next!(stream, SemiColon);
    }
}
