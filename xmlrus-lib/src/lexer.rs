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
    Ampersand,
    /// `#`
    Pound,
    /// Characters between a `&` and `'`
    ReferenceValue(&'a str),

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

    /// XML Version
    Version,
    /// XML Encoding
    Encoding,
    /// XML Standalone doc
    Standalone,

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
            Self::MarkupDeclEnd => write!(f, "MarkupDeclEnd"),
            Self::CDStart => write!(f, "CDStart"),
            Self::CDEnd => write!(f, "CDEnd"),
            Self::ElementDecl => write!(f, "ElementDecl"),
            Self::EntityDecl => write!(f, "EntityDecl"),
            Self::AttlistDecl => write!(f, "AttlistDecl"),
            Self::NotationDecl => write!(f, "NotationDecl"),
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
            Self::Ampersand => write!(f, "Ampersand"),
            Self::Pound => write!(f, "Pound"),
            Self::ReferenceValue(value) => write!(f, "ReferenceValue(\"{value}\")"),
            Self::CData => write!(f, "CData)"),
            Self::TokenizedType(tokenized_type) => write!(f, "TokenizedType(\"{tokenized_type:?}\")"),
            Self::Enumeration => write!(f, "Enumeration"),
            Self::OpenTagStart => write!(f, "OpenTagStart"),
            Self::TagEndStart => write!(f, "TagEndStart"),
            Self::EmptyTagEnd => write!(f, "EmptyTagEnd"),
            Self::TagEnd => write!(f, "TagEnd"),
            Self::AttributeValue(value) => write!(f, "AttributeValue(\"{value}\")"),
            Self::CharData(char_data) => write!(f, "CharData(\"{char_data:#?}\")"), //, char_data.replace("\n", "")),
            Self::Percent => write!(f, "Percent"),
            Self::SemiColon => write!(f, "SemiColon"),
            Self::Comment(comment) => write!(f, "Comment(\"{comment}\")"),
            Self::Equal => write!(f, "Equal"),
            Self::Name(name) => write!(f, "Name(\"{name}\")"),
            Self::NmToken(nm_token) => write!(f, "NmToken(\"{nm_token}\")"),
            Self::PIStart => write!(f, "PIStart"),
            Self::PIEnd => write!(f, "PIEnd"),
            Self::Version => write!(f, "Version"),
            Self::Encoding => write!(f, "Encoding"),
            Self::Standalone => write!(f, "Standalone"),
            Self::SingleQuote => write!(f, "SingleQuote"),
            Self::DoubleQuote => write!(f, "DoubleQuote"),
            Self::Whitespace(_) => write!(f, "Whitespace"),
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
    GEDecl,
    PEDecl,
    NDataDecl,
    NotationType,
    Enumeration,
    ExternalId,
    ContentSpec,
    ElementStart,
    ElementEnd,
    Attributes,
    Reference,
    CData,
}

pub struct Lexer<'a> {
    stream: InputStream<'a>,
    current_token: Token<'a>,
    prev_token: Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut stream = InputStream::new(source);
        let current_token = stream.next().unwrap_or(Token::new(TokenKind::Eof, source.len()));

        Self {
            stream,
            current_token,
            prev_token: None,
        }
    }

    /// Pulls the next token and returns the current
    pub fn next_token(&mut self) -> Token<'a> {
        match self.stream.next() {
            Some(token) => {
                let current = self.current_token;
                self.prev_token = Some(current);
                self.current_token = token;
                println!("{current:?}");
                token
            }
            None => {
                let eof = Token::new(TokenKind::Eof, self.stream.pos);
                self.prev_token = Some(self.current_token);
                self.current_token = eof;
                eof
            }
        }
    }

    pub fn prev_token(&self) -> Option<Token<'a>> {
        self.prev_token
    }

    pub fn current_token(&self) -> Token<'a> {
        self.current_token
    }
}

pub struct InputStream<'a> {
    source: &'a str,
    pos: usize,
    state: State,
    prev_state: State,
}

impl<'a> InputStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            state: State::Default,
            prev_state: State::Default,
        }
    }

    fn set_state(&mut self, new_state: State) {
        // println!("set_state: {:?} -> {:?}", self.state, new_state);
        self.prev_state = self.state;
        self.state = new_state;
    }

    fn advance(&mut self, amount: usize) {
        self.pos += amount;
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
        // println!("    TokenKind::{:?}", kind);
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
                    b' ' | b'\t' | b'\r' | b'\n' => self.chomp_ws(),
                    b'<' if self.starts_with("<?xml") => {
                        self.set_state(State::XmlDecl);
                        self.chomp_back(TokenKind::XmlDeclStart, 5)
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
                        self.set_state(State::Reference);
                        self.next()
                    }
                    _ => {
                        let offset = self.pos;
                        let char_data = self.chomp_char_data()?;
                        self.chomp(TokenKind::CharData(char_data), offset)
                    }
                },
                State::XmlDecl => match b {
                    q @ (b'\'' | b'"') => self.chomp_literal(q),
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'=' => self.chomp_single(TokenKind::Equal),
                    b'?' if self.starts_with("?>") => {
                        self.set_state(State::Default);
                        self.chomp_back(TokenKind::XmlDeclEnd, 2)
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        let kind = match name {
                            "version" => TokenKind::Version,
                            "encoding" => TokenKind::Encoding,
                            "standalone" => TokenKind::Standalone,
                            _ => TokenKind::Name(name),
                        };
                        self.chomp(kind, offset)
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

                    self.advance(3);
                    let comment = self.slice_from(offset);
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
                                b'?' if self.starts_with("?>") => break,
                                b if self.is_ws(b) && !target_chomped => break,
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
                    q @ (b'\'' | b'"') => self.chomp_literal(q),
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
                    b'%' => self.chomp_single(TokenKind::Percent),
                    b';' => self.chomp_single(TokenKind::SemiColon),
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
                    q @ (b'\'' | b'"') => {
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
                    _ => {
                        self.set_state(State::AttlistDecl);
                        self.chomp_single(TokenKind::Pound)
                    }
                },
                State::EntityDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'>' => {
                        self.set_state(State::IntSubset);
                        self.chomp_single(TokenKind::MarkupDeclEnd)
                    }
                    b'%' => {
                        self.state = State::PEDecl;
                        self.next()
                    }
                    _ => {
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.state = State::GEDecl;
                        self.chomp(TokenKind::Name(name), offset)
                    }
                },
                State::PEDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'%' => self.chomp_single(TokenKind::Percent),
                    b'>' => {
                        self.set_state(State::EntityDecl);
                        self.next()
                    }
                    q @ (b'\'' | b'"') => {
                        self.set_state(State::EntityDecl);
                        self.chomp_literal(q)
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
                State::GEDecl => match b {
                    b if self.is_ws(b) => self.chomp_ws(),
                    b'>' => {
                        self.set_state(State::EntityDecl);
                        self.next()
                    }
                    q @ (b'\'' | b'"') => {
                        self.set_state(State::EntityDecl);
                        self.chomp_literal(q)
                    }
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
                        let offset = self.pos;
                        let name = self.chomp_name()?;
                        self.chomp(TokenKind::Name(name), offset)
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
                    q @ (b'\'' | b'"') => self.chomp_literal(q),
                    b if self.starts_with("SYSTEM") => self.chomp_back(TokenKind::System, 6),
                    b if self.starts_with("PUBLIC") => self.chomp_back(TokenKind::Public, 6),
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
                    b'%' => self.chomp_single(TokenKind::Percent),
                    b';' => self.chomp_single(TokenKind::SemiColon),
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
                    q @ (b'\'' | b'"') => self.chomp_literal(q),
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
                State::Reference => match b {
                    b'&' => self.chomp_single(TokenKind::Ampersand),
                    b'#' => self.chomp_single(TokenKind::Pound),
                    b';' => {
                        self.set_state(self.prev_state);
                        self.chomp_single(TokenKind::SemiColon)
                    }
                    _ => {
                        let offset = self.pos;

                        loop {
                            match self.current_byte() {
                                None | Some(b';') => break,
                                Some(_) => self.advance(1),
                            }
                        }

                        let value = self.slice_from(offset);
                        self.chomp(TokenKind::ReferenceValue(value), offset)
                    }
                },
            },
        }
    }
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

    fn with_state(source: &'static str, state: State) -> InputStream<'static> {
        InputStream {
            source,
            pos: 0,
            state,
            prev_state: State::Default,
        }
    }

    #[test]
    fn test_xml_decl() {
        let mut stream = InputStream::new(r#"<?xml version="1.0" encoding="UTF-8"?>"#);

        next!(stream, XmlDeclStart);
        next!(stream, Whitespace(" "));
        next!(stream, Version);
        next!(stream, Equal);
        next!(stream, Literal("1.0"));
        next!(stream, Whitespace(" "));
        next!(stream, Encoding);
        next!(stream, Equal);
        next!(stream, Literal("UTF-8"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_empty() {
        let mut stream = InputStream::new(r#"<?xml?>"#);

        next!(stream, XmlDeclStart);
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_empty_whitespace() {
        let mut stream = InputStream::new("<?xml\t\t\n?>");

        next!(stream, XmlDeclStart);
        next!(stream, Whitespace("\t\t\n"));
        next!(stream, XmlDeclEnd);
    }

    #[test]
    fn test_xml_decl_random_things() {
        let mut stream = InputStream::new(r#"<?xml foo bar baz?>"#);

        next!(stream, XmlDeclStart);
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
        let mut stream = with_state(r#"<!ENTITY name "some name">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("some name"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_pe_decl_entity_value() {
        let mut stream = with_state(r#"<!ENTITY % foo "bar">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_pe_decl_external_id_system() {
        let mut stream = with_state(r#"<!ENTITY % foo SYSTEM "bar">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_pe_decl_external_id_public() {
        let mut stream = with_state(r#"<!ENTITY % foo PUBLIC "one" " two ">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("one"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal(" two "));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_pe_decl_external_id_no_second_literal() {
        let mut stream = with_state(r#"<!ENTITY % foo PUBLIC "one">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("one"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_pe_decl_no_space() {
        let mut stream = with_state(r#"<!ENTITY%foo"bar">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Percent);
        next!(stream, Name("foo"));
        next!(stream, Literal("bar"));
    }

    #[test]
    fn test_pe_decl_percents() {
        let mut stream = with_state(r#"<!ENTITY % % % foo "bar">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
    }

    #[test]
    fn test_pe_decl_single_quotes() {
        let mut stream = with_state(r#"<!ENTITY % foo 'single quotes???'>"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("single quotes???"));
    }

    #[test]
    fn test_gedecl_simple() {
        let mut stream = with_state(r#"<!ENTITY name "some name">"#, State::IntSubset);

        next!(stream, EntityDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("some name"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_gedecl_simple_extra_whitespace() {
        let mut stream = with_state(r#"<!ENTITY name "some name"   >"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name SYSTEM "foo">"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name PUBLIC "foo" SYSTEM "bar">"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name SYSTEM "foo" SYSTEM "bar" SYSTEM>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name SYSTEM "foo" NDATA gif>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name SYSTEM "foo" PUBLIC "bar">"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ENTITY name SYSTEMSYSTEM SYSTEM>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST two chapter CDATA #REQUIRED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr ID #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr IDREF #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr IDREFS #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr ENTITY #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr ENTITIES #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr NMTOKEN #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST a attr NMTOKENS #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST b att NOTATION (a|b) #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST b att NOTATION (  a | b    ) #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST b att NOTATION #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST b att NOTATION () #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ATTLIST b att (a|b) #IMPLIED>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!NOTATION name SYSTEM "name.txt">"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!NOTATION JPGformat PUBLIC "jpg 1" "jpg 2">"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!NOTATION a PUBLIC "b"  >"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ELEMENT name EMPTY>"#, State::IntSubset);

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Empty);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_any() {
        let mut stream = with_state(r#"<!ELEMENT name ANY>"#, State::IntSubset);

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Any);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_any_bad_casing() {
        let mut stream = with_state(r#"<!ELEMENT name any>"#, State::IntSubset);

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("name"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("any"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_element_decl_mixed() {
        let mut stream = with_state(r#"<!ELEMENT name (#PCDATA)>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ELEMENT name (#PCDATA|a|b|c)>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ELEMENT name #PCDATA>"#, State::IntSubset);

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
        let mut stream = with_state(r#"<!ELEMENT div1 (head, (p | list | note)*, div2*)>"#, State::IntSubset);

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
    fn test_element_decl_pe_ref() {
        // Parameter Entity References are illegal here but the lexer will not care and
        // the parser will catch the error.
        let mut stream = with_state(r#"<!ELEMENT div2 (%pe3; | %pe2;)*>"#, State::IntSubset);

        next!(stream, ElementDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("div2"));
        next!(stream, Whitespace(" "));
        next!(stream, LeftParen);
        next!(stream, Percent);
        next!(stream, Name("pe3"));
        next!(stream, SemiColon);
        next!(stream, Whitespace(" "));
        next!(stream, Pipe);
        next!(stream, Whitespace(" "));
        next!(stream, Percent);
        next!(stream, Name("pe2"));
        next!(stream, SemiColon);
        next!(stream, RightParen);
        next!(stream, Star);
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_entity_ref() {
        let mut stream = InputStream::new(r#"&da_ref;"#);

        next!(stream, Ampersand);
        next!(stream, ReferenceValue("da_ref"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_entity_ref_double_ampersand() {
        let mut stream = InputStream::new(r#"&&da_ref;"#);

        next!(stream, Ampersand);
        next!(stream, Ampersand);
        next!(stream, ReferenceValue("da_ref"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_entity_ref_multiple_semi() {
        let mut stream = InputStream::new(r#"&da_ref;;"#);

        next!(stream, Ampersand);
        next!(stream, ReferenceValue("da_ref"));
        next!(stream, SemiColon);
        assert!(stream.next().is_none()) // EOF reached while parsing `CharData`
    }

    #[test]
    fn test_char_ref_decimal() {
        let mut stream = InputStream::new(r#"&#123;"#);

        next!(stream, Ampersand);
        next!(stream, Pound);
        next!(stream, ReferenceValue("123"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_char_ref_decimal_invalid_digit() {
        let mut stream = InputStream::new(r#"&#123f;"#);

        next!(stream, Ampersand);
        next!(stream, Pound);
        next!(stream, ReferenceValue("123f"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_char_ref_decimal_invalid_digit_correction() {
        let mut stream = InputStream::new(r#"<a>&#123f;</a>"#);

        next!(stream, OpenTagStart);
        next!(stream, Name("a"));
        next!(stream, TagEnd);
        next!(stream, Ampersand);
        next!(stream, Pound);
        next!(stream, ReferenceValue("123f"));
        next!(stream, SemiColon);
        next!(stream, TagEndStart);
        next!(stream, Name("a"));
        next!(stream, TagEnd);
    }

    #[test]
    fn test_char_ref_hexadecimal() {
        let mut stream = InputStream::new(r#"&#xff1233;"#);

        next!(stream, Ampersand);
        next!(stream, Pound);
        next!(stream, ReferenceValue("xff1233"));
        next!(stream, SemiColon);
    }

    #[test]
    fn test_notation_decl_system() {
        let mut stream = with_state(r#"<!NOTATION foo SYSTEM "bar">"#, State::IntSubset);

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_decl_mixed() {
        let mut stream = with_state(r#"<!NOTATION foo SYSTEM PUBLICPUBLIC "bar">"#, State::IntSubset);

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, System);
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_decl_public() {
        let mut stream = with_state(r#"<!NOTATION foo PUBLIC "bar" "baz">"#, State::IntSubset);

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Public);
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("baz"));
        next!(stream, MarkupDeclEnd);
    }

    #[test]
    fn test_notation_no_system_public() {
        let mut stream = with_state(r#"<!NOTATION foo PUB "bar" "baz">"#, State::IntSubset);

        next!(stream, NotationDecl);
        next!(stream, Whitespace(" "));
        next!(stream, Name("foo"));
        next!(stream, Whitespace(" "));
        next!(stream, Name("PUB"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("bar"));
        next!(stream, Whitespace(" "));
        next!(stream, Literal("baz"));
        next!(stream, MarkupDeclEnd);
    }
}
