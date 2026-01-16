use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::str::FromStr;

use error::Error;
use error::ParseResult;
use error::SyntaxError;
use error::ValidationError;

pub mod error;
mod pretty;
mod validate;

const XML_PREFIX: &str = "xml";
const XML_URI: &str = "http://www.w3.org/XML/1998/namespace";
const XMLNS_PREFIX: &str = "xmlns";
const XMLNS_URI: &str = "http://www.w3.org/2000/xmlns/";

#[derive(Clone, Debug, Default)]
pub struct Document<'a> {
    pub name: Option<&'a str>,
    pub root_node_id: Option<usize>,
    pub nodes: Vec<Node<'a>>,
}

impl<'a> Document<'a> {
    pub fn get_elements_by_name(&'a self, tag_name: &'a str) -> Vec<&'a Node<'a>> {
        fn walk<'a>(nodes: &'a Vec<Node<'a>>, tag_name: &'a str) -> Vec<&'a Node<'a>> {
            let mut elements = Vec::new();

            for node in nodes {
                if let NodeKind::Element { name, children, .. } = &node.data {
                    if name.local == tag_name {
                        elements.push(node);
                    }

                    if !children.is_empty() {
                        elements.extend(walk(children, tag_name));
                    }
                }
            }

            elements
        }

        walk(&self.nodes, tag_name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Notation<'a> {
    system_id: Option<&'a str>,
    public_id: Option<&'a str>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Entity<'a> {
    name: &'a str,
    entity_type: EntityType<'a>,
    expanding: bool,
}

impl<'a> Entity<'a> {
    fn new(name: &'a str, entity_type: EntityType<'a>) -> Self {
        Self {
            name,
            entity_type,
            expanding: false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EntityType<'a> {
    InternalGeneral {
        value: &'a str,
    },
    InternalParameter {
        value: &'a str,
    },
    // TODO
    InternalPredefined {
        value: char,
    },
    ExternalGeneralParsed {
        system_id: &'a str,
        public_id: Option<&'a str>,
    },
    ExternalGeneralUnparsed {
        system_id: &'a str,
        public_id: Option<&'a str>,
        ndata: &'a str,
    },
    ExternalParameter {
        system_id: &'a str,
        public_id: Option<&'a str>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct QName<'a> {
    prefix: Option<&'a str>,
    local: &'a str,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Attribute<'a> {
    pub qname: QName<'a>,
    pub value: &'a str,
}

impl std::fmt::Debug for Attribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Attribute {{ name: ")?;
        if let Some(prefix) = self.qname.prefix {
            write!(f, "{}:", prefix)?;
        }
        write!(f, "{}, value: {} }}", self.qname.local, self.value)
    }
}

#[derive(Clone, PartialEq)]
pub struct Namespace<'a> {
    pub name: Option<&'a str>,
    pub uri: &'a str,
}

impl<'a> Namespace<'a> {
    fn new(name: Option<&'a str>, uri: &'a str) -> Self {
        Self { name, uri }
    }
}

impl std::fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Namespace {{ name: {:?}, uri: {} }}", self.name, self.uri,)
    }
}

#[derive(Clone, PartialEq)]
pub struct Node<'a> {
    pub id: usize,
    parent: Option<usize>,
    pub data: NodeKind<'a>,
}

impl<'a> Node<'a> {
    fn new(id: usize, parent: Option<usize>, data: NodeKind<'a>) -> Self {
        Self { id, parent, data }
    }
}

impl std::fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]{:#?}", self.id, self.data)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind<'a> {
    Declaration {
        version: &'a str,
        encoding: Option<&'a str>,
        standalone: Option<&'a str>,
    },
    Element {
        name: QName<'a>,
        attributes: Vec<Attribute<'a>>,
        namespaces: Vec<Namespace<'a>>,
        children: Vec<Node<'a>>,
    },
    ProcessingInstruction {
        target: &'a str,
        data: Option<&'a str>,
    },
    Text(&'a str),
    Comment(&'a str),
    CData(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
struct ElementTypeDecl<'a> {
    name: &'a str,
    raw: Option<&'a str>,
    content_spec: ContentSpec<'a>,
}

#[derive(Clone, Debug, PartialEq)]
enum ContentSpec<'a> {
    Empty,
    Any,
    MixedContent(Vec<&'a str>, bool),
    ElementContent(ElementContent<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ElementContent<'a> {
    children: ElementContentChildren<'a>,
    repetition: Repetition,
}

#[derive(Clone, Debug, PartialEq)]
enum ContentParticle<'a> {
    Name {
        name: &'a str,
        repetition: Repetition,
    },
    Choice {
        content: Vec<ContentParticle<'a>>,
        repetition: Repetition,
    },
    Seq {
        content: Vec<ContentParticle<'a>>,
        repetition: Repetition,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum ElementContentChildren<'a> {
    Choice(Vec<ContentParticle<'a>>),
    Seq(Vec<ContentParticle<'a>>),
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Repetition {
    // one or more
    Plus,
    // zero or more
    Star,
    // zero or one
    QuestionMark,
    // once
    Once,
}

impl std::fmt::Display for Repetition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Repetition::Plus => write!(f, "+"),
            Repetition::Star => write!(f, "*"),
            Repetition::QuestionMark => write!(f, "?"),
            Repetition::Once => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct AttributeDecl<'a> {
    element_name: &'a str,
    att_defs: Option<Vec<AttributeDef<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
struct AttributeDef<'a> {
    name: &'a str,
    att_type: AttType<'a>,
    default_decl: DefaultDecl<'a>,
}

#[derive(Clone, Debug, PartialEq)]
enum AttType<'a> {
    String,
    Tokenized(TokenizedType),
    Enumerated(EnumeratedType<'a>),
}

#[derive(Clone, Debug, PartialEq)]
enum TokenizedType {
    Id,
    IdRef,
    IdRefs,
    Entity,
    Entities,
    NmToken,
    NmTokens,
}

#[derive(Clone, Debug, PartialEq)]
enum EnumeratedType<'a> {
    NotationType(Vec<&'a str>),
    Enumeration(Vec<&'a str>),
}

#[derive(Clone, Debug, PartialEq)]
enum DefaultDecl<'a> {
    Required,
    Implied,
    Fixed { fixed: bool, value: &'a str },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ElementType {
    Root,
    Child,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ElementEndKind<'a> {
    /// `>`
    Open,
    /// `/>`
    Empty,
    /// `</ns:tagname>`
    Close { prefix: Option<&'a str>, local: &'a str },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'a> {
    /// ```xml
    /// <ns:name>hello world</ns:name>
    /// ^^^^^^^^
    /// ```
    ElementStart { prefix: Option<&'a str>, local: &'a str },

    /// ```xml
    /// <!-- ElementEndKind::Close -->
    /// <ns:name>hello world</ns:name>
    ///                     ^^^^^^^^^^
    ///
    /// <!-- ElementEndKind::Open -->
    /// <name>hello world</name>
    ///      ^
    ///
    /// <!-- ElementEndKind::Empty -->
    /// <name hello="world"/>
    ///                    ^^
    /// ```
    ElementEnd { kind: ElementEndKind<'a> },

    /// ```xml
    /// <?target instructions?>
    ///   ^^^^^^                 target: Required
    ///          ^^^^^^^^^^^^    instructions: Optional
    /// ```
    ProcessingInstruction { target: &'a str, data: Option<&'a str> },

    /// ```xml
    /// <?xml version="1.0" encoding="UTF-8"?>
    /// ^^^^^
    /// ```
    Declaration {
        version: &'a str,
        encoding: Option<&'a str>,
        standalone: Option<&'a str>,
    },

    /// ```xml
    /// <tagname ns:foo="bar" />
    ///          ^^                prefix
    ///             ^^^            local
    ///                  ^^^       value
    /// ```
    Attribute {
        prefix: Option<&'a str>,
        local: &'a str,
        value: &'a str,
    },

    /// ```xml
    /// <tag>This is some text</tag>
    ///      ^^^^^^^^^^^^^^^^^
    /// ```
    Text { text: &'a str },

    /// ```xml
    /// <!-- comment goes here -->
    /// ```
    Comment { comment: &'a str },

    /// ```xml
    /// <![CDATA[ Data that can <data>markup</data> ]]>
    /// ```
    CData { data: &'a str },
}

pub struct TokenStream<'a> {
    source: &'a str,
    pos: usize,
    len: usize,
}

impl<'a> TokenStream<'a> {
    fn advance(&mut self, amount: usize) {
        self.pos += amount;
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.len
    }

    fn current_byte(&mut self) -> ParseResult<u8> {
        if self.is_at_end() {
            return Err(Error::eof());
        }

        Ok(self.source.as_bytes()[self.pos])
    }

    // First Code Point  Last Code Point  Byte 1    Byte 2    Byte 3    Byte 4
    // U+0000            U+007F           0yyyzzzz
    // U+0080            U+07FF           110xxxyy  10yyzzzz
    // U+0800            U+FFFF           1110wwww  10xxxxyy  10yyzzzz
    // U+010000          U+10FFFF         11110uvv  10vvwwww  10xxxxyy  10yyzzzz
    pub fn current_char(&mut self) -> ParseResult<char> {
        let mut res = 0u32;
        let len = match self.current_byte()? {
            b if b < 0x80 => 1,
            b if b < 0xe0 => 2,
            b if b < 0xf0 => 3,
            _ => 4,
        };

        if self.pos + len > self.len {
            return Err(Error::eof());
        }

        let pos = self.pos;
        let bytes = self.source.as_bytes();
        match len {
            1 => return Ok(bytes[pos] as char),
            2 => {
                res |= (bytes[pos] as u32 & 0x1f) << 6;
                res |= (bytes[pos + 1] as u32) & 0x3f;
            }
            3 => {
                res |= (bytes[pos] as u32 & 0x0f) << 12;
                res |= (bytes[pos + 1] as u32 & 0x3f) << 6;
                res |= bytes[pos + 2] as u32 & 0x3f;
            }
            _ => {
                res |= (bytes[pos] as u32 & 0x07) << 18;
                res |= (bytes[pos + 1] as u32 & 0x3f) << 12;
                res |= (bytes[pos + 2] as u32 & 0x3f) << 6;
                res |= bytes[pos + 3] as u32;
            }
        }

        // Who needs validation
        char::from_u32(res).ok_or(Error::syntax(SyntaxError::InvalidChar { value: res }))
    }

    fn unchecked_current_byte(&mut self) -> u8 {
        self.source.as_bytes()[self.pos]
    }

    fn peek_byte(&mut self) -> Option<u8> {
        if self.pos + 1 >= self.len {
            return None;
        }

        Some(self.source.as_bytes()[self.pos + 1])
    }

    fn peek_prev(&mut self) -> Option<u8> {
        if self.pos == 0 {
            return None;
        }

        Some(self.source.as_bytes()[self.pos - 1])
    }

    fn peek_byte_at(&mut self, pos: usize) -> Option<u8> {
        if self.pos + pos >= self.len {
            return None;
        }

        Some(self.source.as_bytes()[self.pos + pos])
    }

    fn peek_seq(&mut self, seq: &str) -> bool {
        let end = self.pos + seq.len();
        if end > self.len {
            return false;
        }

        self.source[self.pos..end] == *seq
    }

    fn starts_with(&self, prefix: &str) -> bool {
        if self.pos >= self.len {
            return false;
        }

        self.source[self.pos..self.len].starts_with(prefix)
    }

    fn slice(&self, start: usize, end: usize) -> &'a str {
        &self.source[start..end]
    }

    fn slice_from(&self, start: usize) -> &'a str {
        &self.source[start..self.pos]
    }

    // TODO: What do do with you?
    // fn span(&self, start: usize, end: usize) -> Span {
    //     Span::new(self.source, start, end)
    // }
    //
    // fn span_from(&self, start: usize) -> Span {
    //     Span::new(self.source, start, self.pos)
    // }
    //
    // fn span_single(&self) -> Span {
    //     Span::new(self.source, self.pos, self.pos + 1)
    // }

    fn is_white_space(&mut self) -> ParseResult<bool> {
        let current = self.current_byte()?;
        Ok(matches!(current, 0x20 | 0x9 | 0xD | 0xA))
    }

    fn has_preceeding_whitespace(&mut self) -> bool {
        match self.peek_prev() {
            Some(c) => matches!(c, 0x20 | 0x9 | 0xD | 0xA),
            None => false,
        }
    }

    fn expect_and_consume_whitespace(&mut self, at: &'static str) -> ParseResult<()> {
        if !self.is_white_space()? {
            return Err(Error::syntax(SyntaxError::MissingRequiredWhitespace { at }));
        }

        self.consume_whitespace();

        Ok(())
    }

    fn expect_byte(&mut self, c: u8) -> ParseResult<()> {
        let current = self.current_byte()?;
        if current != c {
            return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                expected: (c as char).into(),
                actual: current as char,
            }));
        }

        self.advance(1);
        Ok(())
    }

    fn consume_whitespace(&mut self) -> usize {
        let start = self.pos;
        while !self.is_at_end() {
            // We know we aren't at the end so this will not panic
            if !self.is_white_space().expect("unexpected EOF") {
                break;
            }

            self.advance(1);
        }

        self.pos - start
    }

    fn consume_quote(&mut self) -> ParseResult<u8> {
        let current = self.current_byte()?;
        if current == b'\'' || current == b'"' {
            self.advance(1);
            return Ok(current);
        }

        Err(Error::syntax(SyntaxError::UnexpectedCharacter {
            expected: "a quote".into(),
            actual: current as char,
        }))
    }
}

pub struct Context<'a> {
    doc: Document<'a>,
    prefixes: HashSet<&'a str>,
    entities: HashMap<&'a str, Entity<'a>>,
    notations: HashMap<&'a str, Notation<'a>>,
    attr_decls: HashMap<&'a str, AttributeDecl<'a>>,
    element_types: Vec<ElementTypeDecl<'a>>,
    current_node_id: usize,
}

impl<'a> Context<'a> {
    fn current_id(&mut self) -> usize {
        let current = self.current_node_id;
        self.current_node_id += 1;
        current
    }

    fn has_prefix(&self, prefix: &str) -> bool {
        if prefix == XMLNS_PREFIX {
            return true;
        }

        self.prefixes.contains(prefix)
    }

    fn push_node(&mut self, node: Node<'a>) {
        self.doc.nodes.push(node)
    }

    fn get_entity(&mut self, name: &'a str) -> ParseResult<&Entity<'a>> {
        self.entities
            .get(name)
            .ok_or(Error::validation(ValidationError::UnknownEntityReference {
                ref_: name.to_string(),
            }))
    }

    fn get_entity_mut(&mut self, name: &'a str) -> ParseResult<&mut Entity<'a>> {
        self.entities
            .get_mut(name)
            .ok_or(Error::validation(ValidationError::UnknownEntityReference {
                ref_: name.to_string(),
            }))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Optionality {
    Optional,
    Required,
}

pub struct Parser;

impl Parser {
    pub fn parse<P>(source: P) -> ParseResult<()>
    where
        P: AsRef<Path>,
    {
        let source = match std::fs::read_to_string(source) {
            Ok(source) => source,
            Err(io_error) => return Err(Error::io(io_error)),
        };

        let _doc = Self::parse_from_str(&source)?;

        // dbg!(_doc);

        Ok(())
    }

    pub fn parse_from_str(source: &str) -> ParseResult<Document<'_>> {
        let doc = Document {
            name: None,
            root_node_id: None,
            nodes: Vec::new(),
        };

        let mut ctx = Context {
            doc,
            prefixes: HashSet::new(),
            entities: HashMap::new(),
            notations: HashMap::new(),
            attr_decls: HashMap::new(),
            element_types: Vec::new(),
            current_node_id: 0,
        };

        let mut stream = TokenStream {
            source,
            len: source.len(),
            pos: 0,
        };

        parse_document(&mut stream, &mut ctx)?;

        pretty::PrettyPrinterBuilder::new(std::io::stdout())
            .print_dtd(false)
            .print_comments(false)
            .print_doc_info(false)
            .pretty_print(&ctx)
            .unwrap();

        Ok(ctx.doc)
    }
}

/// Parses the source input, emitting a stream of tokens to build up the
/// resulting `Document`
///
/// Document  ::=  prolog element Misc*
fn parse_document<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    parse_prolog(stream, ctx)?;

    // Parse any comments, PIs, or whitespace before the root element
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b' ' | b'\t' | b'\n' | b'\r' => {
                stream.advance(1);
            }
            b'<' if stream.starts_with("<?") => {
                let pi = parse_processing_instruction(stream, ctx, None)?;
                ctx.doc.nodes.push(pi);
            }
            b'<' if stream.starts_with("<!--") => {
                let comment = parse_comment(stream, ctx, None)?;
                ctx.doc.nodes.push(comment);
            }
            // Start of the root node, break and parse outside of the loop.
            b'<' => break,
            c => {
                return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                    expected: "<".into(),
                    actual: c as char,
                }));
            }
        }
    }

    stream.consume_whitespace();

    if stream.is_at_end() {
        return Err(Error::syntax(SyntaxError::MissingRoot));
    }

    let root = parse_element(stream, ctx, ElementType::Root, None)?;
    ctx.doc.nodes.push(root);

    // Parse any comments or PIs after the root node
    parse_misc(stream, ctx)?;

    if !stream.is_at_end() {
        return Err(Error::syntax(SyntaxError::UnexpectedElement));
    }

    Ok(())
}

fn parse_name<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    let current = stream.current_char()?;
    if !validate::is_name_start_char(current) {
        return Err(Error::syntax(SyntaxError::InvalidXmlChar { c: current }));
    }

    stream.advance(current.len_utf8());

    loop {
        let c = stream.current_char()?;
        if !validate::is_name_char(c) {
            break;
        }
        stream.advance(c.len_utf8());
    }

    let name = stream.slice_from(start);
    validate::is_valid_name(name)?;

    Ok(name)
}

fn parse_nc_name<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    let current = stream.current_char()?;
    if !validate::is_name_start_char(current) || current == ':' {
        return Err(Error::syntax(SyntaxError::InvalidXmlChar { c: current }));
    }
    stream.advance(current.len_utf8());

    loop {
        let c = stream.current_char()?;
        if !validate::is_name_char(c) || c == ':' {
            break;
        }
        stream.advance(c.len_utf8());
    }

    let name = stream.slice_from(start);
    validate::is_valid_name(name)?;

    Ok(name)
}

fn parse_nm_token<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    loop {
        let current = stream.current_char()?;
        if !validate::is_name_char(current) {
            break;
        }
        stream.advance(current.len_utf8());
    }

    let nm_token = stream.slice_from(start);
    if nm_token.is_empty() {
        return Err(Error::syntax(SyntaxError::InvalidNmToken));
    }

    Ok(nm_token)
}

// prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if stream.starts_with("<?xml") {
        parse_xml_decl(stream, ctx)?;
    }

    parse_misc(stream, ctx)?;

    if stream.peek_seq("<!DOCTYPE") {
        parse_doc_type_decl(stream, ctx)?;
        parse_misc(stream, ctx)?;
    }

    Ok(())
}

// Misc  ::=  Comment | PI | S
fn parse_misc<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b' ' | b'\t' | b'\n' | b'\r' => stream.advance(1),
            b'<' if stream.starts_with("<?") => {
                let pi = parse_processing_instruction(stream, ctx, None)?;
                ctx.doc.nodes.push(pi);
            }
            b'<' if stream.starts_with("<!--") => {
                let comment = parse_comment(stream, ctx, None)?;
                ctx.doc.nodes.push(comment);
            }
            _ => break,
        }
    }

    Ok(())
}

// XMLDecl       ::=   '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
// VersionInfo   ::=   S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
// Eq            ::=   S? '=' S?
// VersionNum    ::=   '1.' [0-9]+
// EncodingDecl  ::=   S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
// EncName       ::=   [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
// SDDecl        ::=   S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn parse_xml_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(5);
    stream.expect_and_consume_whitespace("<?xml")?;

    if !stream.peek_seq("version") {
        return Err(Error::syntax(SyntaxError::MissingVersion));
    }

    stream.advance(7);
    stream.consume_whitespace();
    stream.expect_byte(b'=')?;
    stream.consume_whitespace();

    let version = {
        let delimiter = stream.consume_quote()?;
        if !stream.starts_with("1.") {
            return Err(Error::syntax(SyntaxError::InvalidVersion {
                version: "version must be 1.x".into(),
            }));
        }

        let start = stream.pos;
        stream.advance(2);
        loop {
            match stream.current_byte()? {
                c if c == delimiter => break,
                c if c.is_ascii_digit() => stream.advance(1),
                _ => {
                    let version = stream.slice_from(start).into();
                    return Err(Error::syntax(SyntaxError::InvalidVersion { version }));
                }
            }
        }
        let version = stream.slice_from(start);
        stream.consume_quote()?;
        version
    };

    stream.consume_whitespace();
    let encoding = if stream.starts_with("encoding") {
        if !stream.has_preceeding_whitespace() {
            return Err(Error::syntax(SyntaxError::MissingRequiredWhitespace {
                at: "before encoding",
            }));
        }
        stream.advance(8);
        stream.expect_byte(b'=')?;

        let delimiter = stream.consume_quote()?;
        let start = stream.pos;

        if !stream.current_byte()?.is_ascii_alphabetic() {
            return Err(Error::syntax(SyntaxError::InvalidEncodingName));
        }

        stream.advance(1);

        loop {
            let c = stream.current_byte()?;
            match c {
                c if c == delimiter => break,
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'.' | b'_' | b'-' => stream.advance(1),
                _ => break,
            }
        }

        let name = stream.slice_from(start);
        stream.expect_byte(delimiter)?;
        Some(name)
    } else {
        None
    };

    stream.consume_whitespace();
    let standalone = if stream.peek_seq("standalone") {
        if !stream.has_preceeding_whitespace() {
            return Err(Error::syntax(SyntaxError::MissingRequiredWhitespace {
                at: "before standalone",
            }));
        }

        stream.advance(10);
        stream.expect_byte(b'=')?;

        let value = parse_attribute_value(stream, ctx)?;
        if value == "yes" || value == "no" {
            Some(value)
        } else {
            return Err(Error::syntax(SyntaxError::InvalidStandlone { value: value.into() }));
        }
    } else {
        None
    };

    // TODO: While it is valid for the declaration to span multiple lines, if there is no '?>'
    // and the next non-whitespace character is the next element, the diagnostics will report
    // the incorrect source line.
    stream.consume_whitespace();
    if !stream.peek_seq("?>") {
        return Err(Error::syntax(SyntaxError::UnclosedDeclaration));
    }

    stream.advance(2);

    let id = ctx.current_id();
    ctx.push_node(Node::new(
        id,
        None,
        NodeKind::Declaration {
            version,
            encoding,
            standalone,
        },
    ));

    Ok(())
}

// PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
// PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    parent: Option<usize>,
) -> ParseResult<Node<'a>> {
    stream.advance(2);
    if let Some(b'x' | b'X') = stream.peek_byte_at(0)
        && let Some(b'm' | b'M') = stream.peek_byte_at(1)
        && let Some(b'l' | b'L') = stream.peek_byte_at(2)
        && let Some(b' ' | b'\t' | b'\r' | b'\n') = stream.peek_byte_at(3)
    {
        return Err(Error::syntax(SyntaxError::UnexpectedDeclaration));
    }

    let target = parse_name(stream)?;

    if let Ok(b'?') = stream.current_byte() {
        match stream.peek_byte() {
            Some(b'>') => {
                stream.advance(2);
                let id = ctx.current_id();
                let pi = Node::new(id, parent, NodeKind::ProcessingInstruction { target, data: None });
                return Ok(pi);
            }
            Some(c) => {
                return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                    expected: '>'.into(),
                    actual: c as char,
                }));
            }
            None => return Err(Error::eof()),
        }
    }

    stream.expect_and_consume_whitespace("PITarget")?;
    let data_start = stream.pos;
    loop {
        if stream.is_at_end() {
            return Err(Error::eof());
        }

        if let Ok(b'?') = stream.current_byte()
            && let Some(b'>') = stream.peek_byte()
        {
            break;
        }

        stream.advance(1);
    }

    let data = stream.slice(data_start, stream.pos);
    validate::is_xml_chars(data)?;
    stream.advance(2);

    let id = ctx.current_id();
    let pi = Node::new(
        id,
        parent,
        NodeKind::ProcessingInstruction {
            target,
            data: Some(data),
        },
    );

    Ok(pi)
}

// doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
// DeclSep     ::=   PEReference | S [WFC: PE Between Declarations]
// intSubset   ::=   (markupdecl | DeclSep)*
// markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn parse_doc_type_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);
    stream.expect_and_consume_whitespace("<!DOCTYPE")?;

    let name = parse_name(stream)?;
    ctx.doc.name = Some(name);
    stream.consume_whitespace();

    if let Some((system_id, public_id)) = parse_external_id(stream, Optionality::Optional)? {
        ctx.entities.insert(
            name,
            Entity::new(name, EntityType::ExternalGeneralParsed { system_id, public_id }),
        );
    }

    stream.consume_whitespace();
    if stream.current_byte()? == b'[' {
        stream.advance(1);

        // Parse 'intSubset'
        loop {
            match stream.current_byte()? {
                b']' => break,
                b'<' if stream.starts_with("<!ELEMENT") => parse_element_type_decl(stream, ctx)?,
                b'<' if stream.starts_with("<!ENTITY") => parse_entity_decl(stream, ctx)?,
                b'<' if stream.starts_with("<!ATTLIST") => parse_attlist_decl(stream, ctx)?,
                b'<' if stream.starts_with("<!NOTATION") => parse_notation_decl(stream, ctx)?,
                b'<' if stream.starts_with("<?") => {
                    let pi = parse_processing_instruction(stream, ctx, None)?;
                    ctx.push_node(pi);
                }
                b'<' if stream.starts_with("<!--") => {
                    let comment = parse_comment(stream, ctx, None)?;
                    ctx.push_node(comment);
                }
                _ if stream.is_white_space()? => stream.advance(1),
                b => {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "'<' or ' '".into(),
                        actual: b as char,
                    }));
                }
            }
        }

        stream.expect_byte(b']')?;
        stream.consume_whitespace();
    }

    stream.expect_byte(b'>')?;

    Ok(())
}

// AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);
    stream.expect_and_consume_whitespace("<!ATTLIST")?;

    let element_name = parse_name(stream)?;
    stream.consume_whitespace();

    let mut att_defs = None;
    loop {
        stream.consume_whitespace();
        match stream.current_byte()? {
            b'>' => break,
            _ => {
                let att_def = parse_att_def(stream, ctx)?;
                match &mut att_defs {
                    None => att_defs = Some(vec![att_def]),
                    Some(att_defs) => att_defs.push(att_def),
                }
            }
        }
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    let att_decl = AttributeDecl { element_name, att_defs };
    ctx.attr_decls.insert(element_name, att_decl);

    Ok(())
}

// AttDef         ::=  S Name S AttType S DefaultDecl
// AttType        ::=  StringType | TokenizedType | EnumeratedType
// StringType     ::=  'CDATA'
// TokenizedType  ::=  'ID'
//                     | 'IDREF'
//                     | 'IDREFS'
//                     | 'ENTITY'
//                     | 'ENTITIES'
//                     | 'NMTOKEN'
//                     | 'NMTOKENS'
fn parse_att_def<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<AttributeDef<'a>> {
    stream.consume_whitespace();
    let name = parse_name(stream)?;
    stream.expect_and_consume_whitespace("AttDef name")?;

    let att_type = {
        if stream.starts_with("CDATA") {
            stream.advance(5);
            AttType::String
        } else if stream.starts_with("ID") {
            stream.advance(2);
            AttType::Tokenized(TokenizedType::Id)
        } else if stream.starts_with("IDREF") {
            stream.advance(5);
            AttType::Tokenized(TokenizedType::IdRef)
        } else if stream.starts_with("IDREFS") {
            stream.advance(6);
            AttType::Tokenized(TokenizedType::IdRefs)
        } else if stream.starts_with("ENTITY") {
            stream.advance(6);
            AttType::Tokenized(TokenizedType::Entity)
        } else if stream.starts_with("ENTITIES") {
            stream.advance(8);
            AttType::Tokenized(TokenizedType::Entities)
        } else if stream.starts_with("NMTOKEN") {
            stream.advance(7);
            AttType::Tokenized(TokenizedType::NmToken)
        } else if stream.starts_with("NMTOKENS") {
            stream.advance(8);
            AttType::Tokenized(TokenizedType::NmTokens)
        } else if stream.starts_with("NOTATION") {
            stream.advance(8);
            stream.expect_and_consume_whitespace("NOTATION")?;
            stream.expect_byte(b'(')?;
            stream.consume_whitespace();

            let name = parse_name(stream)?;
            let mut names = vec![name];

            loop {
                stream.consume_whitespace();
                match stream.current_byte()? {
                    b')' => break,
                    b'|' => {
                        stream.advance(1);
                        stream.consume_whitespace();
                        names.push(parse_name(stream)?);
                    }
                    sep => {
                        return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                            expected: "'|' or ')'".into(),
                            actual: sep as char,
                        }));
                    }
                }
            }
            stream.advance(1);
            AttType::Enumerated(EnumeratedType::NotationType(names))
        } else if stream.starts_with("(") {
            stream.advance(1);
            stream.consume_whitespace();

            let nm_token = parse_nm_token(stream)?;
            let mut nm_tokens = vec![nm_token];

            loop {
                stream.consume_whitespace();
                match stream.current_byte()? {
                    b')' => break,
                    b'|' => {
                        stream.advance(1);
                        stream.consume_whitespace();
                        nm_tokens.push(parse_nm_token(stream)?);
                    }
                    sep => {
                        return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                            expected: "'|' or ')'".into(),
                            actual: sep as char,
                        }));
                    }
                }
            }
            stream.advance(1);
            AttType::Enumerated(EnumeratedType::Enumeration(nm_tokens))
        } else {
            return Err(Error::syntax(SyntaxError::InvalidAttributeType));
        }
    };

    stream.expect_and_consume_whitespace("AttType")?;
    let default_decl = parse_default_decl(stream, ctx)?;

    Ok(AttributeDef {
        name,
        att_type,
        default_decl,
    })
}

// DefaultDecl  ::=  '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
fn parse_default_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<DefaultDecl<'a>> {
    if stream.starts_with("#REQUIRED") {
        stream.advance(9);
        Ok(DefaultDecl::Required)
    } else if stream.starts_with("#IMPLIED") {
        stream.advance(8);
        Ok(DefaultDecl::Implied)
    } else {
        let mut fixed = false;
        if stream.starts_with("#FIXED") {
            fixed = true;
            stream.advance(6);
            stream.expect_and_consume_whitespace("FIXED Default Decl")?;
        }
        let value = parse_attribute_value(stream, ctx)?;
        Ok(DefaultDecl::Fixed { fixed, value })
    }
}

// NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
// PublicID      ::=  'PUBLIC' S PubidLiteral
//
// Validity constraint: Unique Notation Name
fn parse_notation_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(10);
    stream.expect_and_consume_whitespace("<!NOTATION")?;

    let name = parse_name(stream)?;
    if ctx.notations.contains_key(name) {
        return Err(Error::validation(ValidationError::DuplicateNotation {
            name: name.into(),
        }));
    }

    stream.expect_and_consume_whitespace("notation name")?;

    // Three paths we can take
    // 1: SYSTEM SystemLiteral
    // 2: PUBLIC PubidLiteral
    // 3: PUBLIC PubidLiteral SystemLiteral
    if stream.peek_seq("SYSTEM") {
        stream.advance(6);
        stream.expect_and_consume_whitespace("SYSTEM")?;
        let system_id = parse_system_literal(stream)?;
        ctx.notations.insert(
            name,
            Notation {
                system_id: Some(system_id),
                public_id: None,
            },
        );
    } else if stream.peek_seq("PUBLIC") {
        stream.advance(6);
        stream.expect_and_consume_whitespace("PUBLIC")?;

        let public_id = parse_public_id_literal(stream)?;
        stream.consume_whitespace();
        let system_id = match stream.current_byte()? {
            b'\'' | b'"' => {
                if !stream.has_preceeding_whitespace() {
                    return Err(Error::syntax(SyntaxError::MissingRequiredWhitespace {
                        at: "before public id literal",
                    }));
                }

                Some(parse_system_literal(stream)?)
            }
            _ => None,
        };

        ctx.notations.insert(
            name,
            Notation {
                system_id,
                public_id: Some(public_id),
            },
        );
    } else {
        return Err(Error::syntax(SyntaxError::InvalidNotationDecl {
            reason: "missing ExternalId or PublicId",
        }));
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(())
}

// ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(
    stream: &mut TokenStream<'a>,
    optionality: Optionality,
) -> ParseResult<Option<(&'a str, Option<&'a str>)>> {
    if stream.peek_seq("SYSTEM") {
        stream.advance(6);
        stream.expect_and_consume_whitespace("SYSTEM")?;

        let system_literal = parse_system_literal(stream)?;

        return Ok(Some((system_literal, None)));
    }

    if stream.peek_seq("PUBLIC") {
        stream.advance(6);
        stream.expect_and_consume_whitespace("PUBLIC")?;

        let pub_id_literal = parse_public_id_literal(stream)?;
        stream.expect_and_consume_whitespace("public identifier")?;
        let system_literal = parse_system_literal(stream)?;

        return Ok(Some((system_literal, Some(pub_id_literal))));
    }

    if optionality == Optionality::Required {
        Err(Error::syntax(SyntaxError::MissingRequiredExternalId))
    } else {
        Ok(None)
    }
}

// SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn parse_system_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    loop {
        let current = stream.current_byte()?;
        if current == delimiter {
            break;
        }

        stream.advance(1);
    }

    // TODO: dereference to obtain input (this one is going low on the priority list lol)
    let system_identifier = stream.slice_from(start);
    stream.advance(1);
    Ok(system_identifier)
}

// PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
// PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn parse_public_id_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    loop {
        let current = stream.current_byte()?;
        if current == delimiter {
            break;
        }

        if !matches!(current, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b' ' | b'\n' | b'\r' | b'-' | b'\'' | b'(' | b')'
                        | b'+' | b',' | b'.' | b'/' | b':' | b'=' | b'?' | b';' | b'!' | b'*' | b'#' | b'@' | b'$' | b'_' | b'%')
        {
            // TODO actual error
            return Err(Error::syntax(SyntaxError::InvalidPublicIdLiteral {
                c: current as char,
            }));
        }

        stream.advance(1);
    }

    let pubid_literal = stream.slice_from(start);
    stream.advance(1);
    Ok(pubid_literal)
}

// elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
// contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);

    stream.expect_and_consume_whitespace("<!ELEMENT")?;
    let name = parse_name(stream)?;

    stream.expect_and_consume_whitespace("element name")?;

    // An element type MUST NOT be declared more than once.
    if ctx.element_types.iter().any(|el| el.name == name) {
        return Err(Error::validation(ValidationError::DuplicateElementType {
            name: name.into(),
        }));
    }

    if stream.peek_seq("EMPTY") {
        ctx.element_types.push(ElementTypeDecl {
            name,
            raw: None,
            content_spec: ContentSpec::Empty,
        });
        stream.advance(5);
    } else if stream.peek_seq("ANY") {
        ctx.element_types.push(ElementTypeDecl {
            name,
            raw: None,
            content_spec: ContentSpec::Any,
        });
        stream.advance(3);
    } else if stream.peek_seq("(") {
        stream.expect_byte(b'(')?;
        stream.consume_whitespace();

        if stream.peek_seq("#PCDATA") {
            parse_mixed_content(stream, ctx, name)?;
        } else {
            let start = stream.pos - 1;
            let children = parse_element_content_children(stream, ctx)?;
            let repetition = parse_repetition(stream)?;
            let raw = stream.slice_from(start);
            ctx.element_types.push(ElementTypeDecl {
                name,
                raw: Some(raw),
                content_spec: ContentSpec::ElementContent(ElementContent { children, repetition }),
            });
        }
    } else {
        return Err(Error::syntax(SyntaxError::MalformedElementTypeDecl));
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(())
}

// Mixed  ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
fn parse_mixed_content<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>, name: &'a str) -> ParseResult<()> {
    let mut names = vec![];
    let mut repeated = false;
    let start = stream.pos - 1;

    stream.advance(7);
    stream.consume_whitespace();

    if stream.current_byte()? == b')' {
        stream.advance(1);
        if let Ok(b'*') = stream.current_byte() {
            repeated = true;
            stream.advance(1);
        }
    } else {
        loop {
            stream.consume_whitespace();
            stream.expect_byte(b'|')?;
            stream.consume_whitespace();

            let name = parse_name(stream)?;

            // The same name MUST NOT appear more than once in a single mixed-content declaration.
            if names.contains(&name) {
                return Err(Error::validation(ValidationError::DuplicateMixedContent {
                    name: name.into(),
                }));
            }

            names.push(name);
            stream.consume_whitespace();

            if stream.current_byte()? == b')' {
                break;
            }
        }

        stream.advance(1);
        stream.expect_byte(b'*')?;
        repeated = true;
    }

    let raw = stream.slice_from(start);
    ctx.element_types.push(ElementTypeDecl {
        name,
        raw: Some(raw),
        content_spec: ContentSpec::MixedContent(names, repeated),
    });

    Ok(())
}

fn parse_parameter_entity_ref<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    // TODO: Something with me
    stream.expect_byte(b'%')?;
    let name = parse_name(stream)?;
    stream.expect_byte(b';')?;
    Ok(name)
}

// EntityDecl  ::=  GEDecl | PEDecl
// GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
// PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(8);
    stream.expect_and_consume_whitespace("<!ENTITY")?;

    if stream.current_byte()? == b'%' {
        parse_pe_def(stream, ctx)?;
    } else {
        parse_entity_def(stream, ctx)?;
    }

    Ok(())
}

// PEDecl  ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
// PEDef   ::=  EntityValue | ExternalID
fn parse_pe_def<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(1);
    stream.expect_and_consume_whitespace("%")?;

    let name = parse_name(stream)?;
    stream.expect_and_consume_whitespace("parameter entity name")?;

    if let Ok(b'\'' | b'"') = stream.current_byte() {
        // If the same entity is declared more than once, the first declaration encountered is binding
        let value = parse_entity_value(stream, name)?;
        if ctx.entities.contains_key(name) {
            return Ok(());
        }
        ctx.entities
            .insert(name, Entity::new(name, EntityType::InternalGeneral { value }));
    } else if let Some((system_id, public_id)) = parse_external_id(stream, Optionality::Required)? {
        ctx.entities.insert(
            name,
            Entity::new(name, EntityType::ExternalGeneralParsed { system_id, public_id }),
        );
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(())
}

// GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
// EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn parse_entity_def<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    let name = parse_name(stream)?;
    stream.expect_and_consume_whitespace("entity def name")?;

    if let Ok(b'\'' | b'"') = stream.current_byte() {
        // If the same entity is declared more than once, the first declaration encountered is binding
        let value = parse_entity_value(stream, name)?;
        if ctx.entities.contains_key(name) {
            return Ok(());
        }
        ctx.entities
            .insert(name, Entity::new(name, EntityType::InternalGeneral { value }));
    } else {
        let Some((system_id, public_id)) = parse_external_id(stream, Optionality::Required)? else {
            // TODO: Error type?
            return Err(Error::syntax(SyntaxError::MissingRequiredExternalId));
        };

        stream.consume_whitespace();

        if stream.peek_seq("NDATA") {
            if !stream.has_preceeding_whitespace() {
                return Err(Error::syntax(SyntaxError::MissingRequiredWhitespace {
                    at: "before NDATA",
                }));
            }

            let ndata = parse_ndata_decl(stream)?;
            ctx.entities.insert(
                name,
                Entity::new(
                    name,
                    EntityType::ExternalGeneralUnparsed {
                        system_id,
                        public_id,
                        ndata,
                    },
                ),
            );
        } else {
            ctx.entities.insert(
                name,
                Entity::new(name, EntityType::ExternalGeneralParsed { system_id, public_id }),
            );
        }
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(())
}

// NDataDecl  ::=  S 'NDATA' S Name
// If the NDataDecl is present, this is a general unparsed entity; otherwise it is a parsed entity.
// VC: The Name MUST match the declared name of a notation.
fn parse_ndata_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    // TODO: VC
    stream.advance(5);
    stream.expect_and_consume_whitespace("NDATA")?;
    let name = parse_name(stream)?;
    Ok(name)
}

// EntityValue  ::=  '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
fn parse_entity_value<'a>(stream: &mut TokenStream<'a>, _name: &'a str) -> ParseResult<&'a str> {
    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    loop {
        let current_byte = stream.current_byte()?;
        if current_byte >= 0x80 {
            let current_char = stream.current_char()?;
            stream.advance(current_char.len_utf8());
        } else {
            match current_byte {
                c if c == delimiter => break,
                // PEReference  ::= '%' Name ';'
                b'%' => {
                    let pe_ref = parse_parameter_entity_ref(stream)?;
                    return Err(Error::syntax(SyntaxError::IllegalParameteEntityRef {
                        ref_: pe_ref.into(),
                    }));
                }
                // EntityRef or CharRef
                b'&' => {
                    if stream.starts_with("&#") {
                        parse_character_reference(stream)?;
                    } else {
                        stream.advance(1);
                        let _name = parse_name(stream)?;
                        stream.expect_byte(b';')?;
                    }
                }
                _ => stream.advance(1),
            }
        }
    }

    let value = stream.slice(start, stream.pos);
    stream.advance(1);

    Ok(value)
}

fn parse_element<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    kind: ElementType,
    parent: Option<usize>,
) -> ParseResult<Node<'a>> {
    let id = ctx.current_id();
    let mut attributes = vec![];
    let mut namespaces = vec![];
    let mut children = vec![];

    let start_qname = parse_element_start(stream, ctx)?;

    if let Some(doc_name) = ctx.doc.name
        && let ElementType::Root = kind
        && doc_name != start_qname.local
    {
        return Err(Error::validation(ValidationError::InvalidRootElementType {
            expected: doc_name.into(),
            actual: start_qname.local.into(),
        }));
    }

    let mut is_open = false;
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b'>' => {
                stream.advance(1);
                is_open = true;
                break;
            }
            b'/' => {
                stream.advance(1);
                stream.expect_byte(b'>')?;

                return Ok(Node::new(
                    id,
                    parent,
                    NodeKind::Element {
                        name: start_qname,
                        namespaces,
                        attributes,
                        children,
                    },
                ));
            }
            c => {
                // Attributes need a leading white space
                if !stream.is_white_space()? {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "a space".into(),
                        actual: c as char,
                    }));
                }
                stream.consume_whitespace();

                let (qname, value) = parse_attribute(stream, ctx)?;
                let QName { prefix, local } = qname;
                match prefix {
                    // prefixed namespace - 'xmlns:prefix="foobarbaz"'
                    Some(prefix) if prefix == XMLNS_PREFIX => {
                        // 'xml'
                        if local == XML_PREFIX {
                            // 'xml' prefix can only be bound to the 'http://www.w3.org/XML/1998/namespace' namespace
                            if value != XML_URI {
                                return Err(Error::syntax(SyntaxError::InvalidXmlPrefixUri));
                            }
                        }

                        // Prefixes other than `xml` MUST NOT be bound to the `http://www.w3.org/XML/1998/namespace` namespace name
                        if value == XML_URI {
                            return Err(Error::syntax(SyntaxError::UnexpectedXmlUri));
                        }

                        // The 'xmlns' prefix is bound to the 'http://www.w3.org/2000/xmlns/' namespace and MUST NOT be declared
                        if value == XMLNS_URI {
                            return Err(Error::syntax(SyntaxError::UnexpectedXmlnsUri));
                        }

                        if namespaces.iter().any(|ns| ns.name == Some(local)) {
                            return Err(Error::validation(ValidationError::DuplicateNamespace {
                                name: local.into(),
                            }));
                        }

                        ctx.prefixes.insert(local);
                        namespaces.push(Namespace::new(Some(local), value));
                    }
                    // Default/unprefixed namespace - 'xmlns="foobarbaz"'
                    None if local == XMLNS_PREFIX => {
                        // The 'xmlns' prefix is bound to the 'http://www.w3.org/2000/xmlns/' namespace and MUST NOT be declared
                        if value == XMLNS_URI {
                            return Err(Error::syntax(SyntaxError::UnexpectedXmlnsUri));
                        }

                        namespaces.push(Namespace::new(None, value));
                    }
                    // Prefixed Attribute
                    Some(prefix) => {
                        if !ctx.prefixes.contains(prefix) {
                            return Err(Error::validation(ValidationError::UnknownPrefix {
                                name: prefix.into(),
                            }));
                        }

                        if attributes.iter().any(|a: &Attribute| a.qname.local == local) {
                            return Err(Error::syntax(SyntaxError::DuplicateAttribute { name: local.into() }));
                        }

                        attributes.push(Attribute { qname, value });
                    }
                    // Unprefixed Attribute
                    None => {
                        if attributes.iter().any(|a: &Attribute| a.qname.local == local) {
                            return Err(Error::syntax(SyntaxError::DuplicateAttribute { name: local.into() }));
                        }

                        attributes.push(Attribute { qname, value });
                    }
                }
            }
        }
    }

    let prefixes = ctx.prefixes.clone();
    if is_open {
        children.extend(parse_content(stream, ctx, id)?);
    }
    ctx.prefixes = prefixes;

    let end_qname = parse_element_end(stream, ctx)?;
    if start_qname.prefix != end_qname.prefix || start_qname.local != end_qname.local {
        let (expected, actual) = match start_qname.prefix {
            Some(prefix) => (
                format!("{}:{}", prefix, start_qname.local),
                format!("{}:{}", prefix, end_qname.local),
            ),
            None => (start_qname.local.to_string(), end_qname.local.to_string()),
        };
        return Err(Error::syntax(SyntaxError::ElementNameMismatch { expected, actual }));
    }

    // root node
    if parent.is_none() {
        ctx.doc.root_node_id = Some(id);
    }

    let element = Node::new(
        id,
        parent,
        NodeKind::Element {
            name: start_qname,
            namespaces,
            attributes,
            children,
        },
    );

    Ok(element)
}

// STag ::= '<' QName (S Attribute)* S? '>'
//          ^^^^^^^^^
fn parse_element_start<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<QName<'a>> {
    stream.advance(1);

    if stream.is_white_space()? {
        return Err(Error::syntax(SyntaxError::InvalidXmlName));
    }

    let qname = parse_qname(stream, ctx)?;

    if let Some(prefix) = qname.prefix
        && prefix == XMLNS_PREFIX
    {
        return Err(Error::syntax(SyntaxError::ReservedPrefix));
    }

    Ok(qname)
}

// Attribute  ::=  NSAttName Eq AttValue | QName Eq AttValue
// AttValue   ::=  '"' ([^<&"] | Reference)* '"'
//              |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<(QName<'a>, &'a str)> {
    let qname = parse_qname(stream, ctx)?;
    stream.consume_whitespace();
    stream.expect_byte(b'=')?;
    stream.consume_whitespace();
    let value = parse_attribute_value(stream, ctx)?;
    Ok((qname, value))
}

enum StrSlice<'a> {
    Borrowed(&'a str),
    Owned(String),
}

fn parse_attribute_value<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<&'a str> {
    fn normalize_char(c: char, normalized: &mut Vec<u8>) {
        let len = c.len_utf8();
        if len > 1 {
            let mut bytes = vec![0; len];
            c.encode_utf8(&mut bytes);
            normalized.extend_from_slice(&bytes);
        } else {
            normalized.push(c as u8);
        }
    }

    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    let mut normalized = vec![];
    let mut owned = false;

    loop {
        let current = stream.current_char();
        match current {
            Ok(d) if d == delimiter as char => break,
            Ok('\r' | '\n' | '\t') => {
                owned = true;
                normalized.push(b' ');
                stream.advance(1);
            }
            Ok('&') if stream.peek_byte() == Some(b'#') => {
                owned = true;
                let c = parse_character_reference(stream)?;
                normalize_char(c, &mut normalized);
            }
            Ok('&') => {
                owned = true;
                let entity_name = parse_entity_ref(stream)?;
                let entity_type = ctx.get_entity(entity_name)?.entity_type;
                match entity_type {
                    EntityType::InternalGeneral { value } => {
                        if value.contains('<') {
                            return Err(Error::syntax(SyntaxError::UnescapedLTInAttrValue));
                        }

                        if value.contains('&') {
                            ctx.get_entity_mut(entity_name)?.expanding = true;
                            normalized.extend(expand_entity_ref(stream, ctx, value, false)?);
                            ctx.get_entity_mut(entity_name)?.expanding = false;
                        } else {
                            normalized.extend_from_slice(value.as_bytes());
                        }
                    }
                    EntityType::ExternalGeneralUnparsed { .. }
                    | EntityType::ExternalParameter { .. }
                    | EntityType::ExternalGeneralParsed { .. } => {
                        return Err(Error::syntax(SyntaxError::ExternalEntityRefInAttribute {
                            ref_: entity_name.into(),
                        }));
                    }
                    EntityType::InternalParameter { .. } => unimplemented!("internal parameter entity"),
                    EntityType::InternalPredefined { .. } => unimplemented!("internal predefined"),
                }
            }
            Ok('<') => return Err(Error::syntax(SyntaxError::UnescapedLTInAttrValue)),
            Ok(c) if !validate::is_xml_char(c) => return Err(Error::syntax(SyntaxError::InvalidXmlChar { c })),
            Ok(c) => {
                normalize_char(c, &mut normalized);
                stream.advance(c.len_utf8())
            }
            Err(err) => return Err(err),
        }
    }

    // We should only break on the delimiter so this should not attempt to slice in the middle
    // of a non-ascii character.
    let value = stream.slice_from(start);
    stream.advance(1);

    if owned {
        println!(
            "OWNED: {}",
            String::from_utf8(normalized).expect("Attribute value contains invalid UTF-8"),
        );
        // Ok(StrSlice::Owned(
        //     String::from_utf8(normalized).expect("Attribute value contains invalid UTF-8"),
        // ))
    } else {
        println!("BORROWED: {value}");
        // Ok(StrSlice::Borrowed(value))
    }

    Ok(value)
}

fn expand_entity_ref<'a>(
    _stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    entity_ref: &'a str,
    allow_external: bool,
) -> ParseResult<Vec<u8>> {
    // TODO:
    //  - Character Referecnes
    let mut expanded = vec![];
    let mut pos = 0;
    let mut ref_start = 0;
    let mut in_ref = false;

    let bytes = entity_ref.as_bytes();
    loop {
        if pos >= bytes.len() {
            break;
        }

        match bytes[pos] {
            b'&' => {
                if in_ref {
                    return Err(Error::syntax(SyntaxError::MalformedEntityReference {
                        reason: "Illegal character in entity reference '&'",
                    }));
                }
                in_ref = true;
                ref_start = pos;
            }
            b';' => {
                if in_ref {
                    in_ref = false;
                }

                if pos - ref_start <= 1 {
                    return Err(Error::syntax(SyntaxError::MalformedEntityReference {
                        reason: "empty entity reference",
                    }));
                }

                let name = &entity_ref[ref_start + 1..pos];
                let entity = match ctx.entities.get(&name) {
                    Some(entity) => entity,
                    None => {
                        return Err(Error::validation(ValidationError::UnknownEntityReference {
                            ref_: name.to_string(),
                        }));
                    }
                };

                if entity.expanding {
                    return Err(Error::syntax(SyntaxError::RecursiveEntityReference {
                        ref_: entity.name.into(),
                    }));
                }

                match entity.entity_type {
                    EntityType::InternalGeneral { value } => {
                        if value.contains('&') {
                            expanded.extend(expand_entity_ref(_stream, ctx, value, false)?);
                        } else {
                            expanded.extend(value.as_bytes());
                        }
                    }
                    EntityType::ExternalGeneralUnparsed { .. }
                    | EntityType::ExternalParameter { .. }
                    | EntityType::ExternalGeneralParsed { .. } => {
                        if !allow_external {
                            return Err(Error::syntax(SyntaxError::ExternalEntityRefInAttribute {
                                ref_: entity_ref.into(),
                            }));
                        }
                    }
                    EntityType::InternalParameter { .. } => unimplemented!("internal parameter entity"),
                    EntityType::InternalPredefined { .. } => unimplemented!("internal predefined"),
                }
            }
            b => {
                if !in_ref {
                    expanded.push(b);
                }
            }
        }

        pos += 1;
    }

    if in_ref {
        return Err(Error::syntax(SyntaxError::MalformedEntityReference {
            reason: "unclosed entity ref",
        }));
    }

    Ok(expanded)
}

// ETag  ::=  '</' QName S? '>'
fn parse_element_end<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<QName<'a>> {
    stream.advance(2);

    if stream.is_white_space()? {
        return Err(Error::syntax(SyntaxError::InvalidXmlName));
    }

    let qname = parse_qname(stream, ctx)?;

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(qname)
}

// QName           ::=  PrefixedName | UnprefixedName
// PrefixedName    ::=  Prefix ':' LocalPart
// UnprefixedName  ::=  LocalPart
// Prefix          ::=  NCName
// LocalPart       ::=  NCName
// NCName          ::=  Name - (Char* ':' Char*) /* An XML Name, minus the ":" */
fn parse_qname<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<QName<'a>> {
    let local = parse_nc_name(stream)?;

    if stream.current_byte()? != b':' {
        if local.is_empty() {
            return Err(Error::syntax(SyntaxError::InvalidXmlName));
        }
        return Ok(QName { prefix: None, local });
    }

    let prefix = local;
    if prefix.is_empty() {
        return Err(Error::syntax(SyntaxError::InvalidXmlName));
    }

    // 'xml' prefix will be mapped to 'http://www.w3.org/XML/1998/namespace'
    if prefix != XML_PREFIX && !ctx.has_prefix(prefix) {
        return Err(Error::validation(ValidationError::UnknownPrefix {
            name: prefix.into(),
        }));
    }

    stream.expect_byte(b':')?;

    let local = parse_nc_name(stream)?;
    if local.is_empty() {
        return Err(Error::syntax(SyntaxError::InvalidXmlName));
    }

    Ok(QName {
        prefix: Some(prefix),
        local,
    })
}

// content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    parent_id: usize,
) -> ParseResult<Vec<Node<'a>>> {
    let mut content = vec![];
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b'<' if stream.starts_with("<?") => {
                content.push(parse_processing_instruction(stream, ctx, Some(parent_id))?)
            }
            b'<' if stream.starts_with("<![CDATA[") => content.push(parse_cdata(stream, ctx, Some(parent_id))?),
            b'<' if stream.starts_with("<!--") => content.push(parse_comment(stream, ctx, Some(parent_id))?),
            b'<' if stream.starts_with("</") => break,
            b'&' => {
                let entity_name = parse_entity_ref(stream)?;
                let entity_type = ctx.get_entity(entity_name)?.entity_type;
                match entity_type {
                    EntityType::InternalGeneral { value } => {
                        if value.contains('<') {
                            return Err(Error::syntax(SyntaxError::UnescapedLTInAttrValue));
                        }
                        if value.contains('&') {
                            ctx.get_entity_mut(entity_name)?.expanding = true;
                            let _ = expand_entity_ref(stream, ctx, value, true)?;
                            ctx.get_entity_mut(entity_name)?.expanding = false;
                        }
                    }
                    EntityType::ExternalGeneralUnparsed { .. }
                    | EntityType::ExternalParameter { .. }
                    | EntityType::ExternalGeneralParsed { .. } => {
                        return Err(Error::syntax(SyntaxError::IllegalUnparsedEntity {
                            ref_: entity_name.into(),
                        }));
                    }
                    EntityType::InternalParameter { .. } => unimplemented!("internal parameter entity"),
                    EntityType::InternalPredefined { .. } => unimplemented!("internal predefined"),
                }
            }
            b'<' => content.push(parse_element(stream, ctx, ElementType::Child, Some(parent_id))?),
            _ => {
                if stream.is_white_space()? {
                    stream.advance(1);
                } else {
                    let text = parse_text(stream)?;
                    let id = ctx.current_id();
                    let node = Node::new(id, Some(parent_id), NodeKind::Text(text));
                    content.push(node)
                }
            }
        }
    }

    Ok(content)
}

fn parse_character_reference<'a>(stream: &mut TokenStream<'a>) -> ParseResult<char> {
    let prefix_len = if stream.peek_seq("&#x") { 3 } else { 2 };
    stream.advance(prefix_len);
    let start = stream.pos;

    let u32_value = if prefix_len == 3 {
        loop {
            match stream.current_byte()? {
                b';' => break,
                b if b.is_ascii_hexdigit() => stream.advance(1),
                _ => {
                    return Err(Error::syntax(SyntaxError::InvalidCharRef {
                        reason: "invalid hexadecimal value".into(),
                    }));
                }
            }
        }

        match u32::from_str_radix(stream.slice_from(start), 16) {
            Ok(value) => value,
            Err(err) => {
                return Err(Error::syntax(SyntaxError::InvalidCharRef {
                    reason: err.to_string(),
                }));
            }
        }
    } else {
        loop {
            match stream.current_byte()? {
                b';' => break,
                b if b.is_ascii_digit() => stream.advance(1),
                _ => {
                    return Err(Error::syntax(SyntaxError::InvalidCharRef {
                        reason: "invalid decimal value".into(),
                    }));
                }
            }
        }
        match u32::from_str(stream.slice_from(start)) {
            Ok(value) => value,
            Err(err) => {
                return Err(Error::syntax(SyntaxError::InvalidCharRef {
                    reason: err.to_string(),
                }));
            }
        }
    };

    let c = match char::from_u32(u32_value) {
        Some(c) => c,
        None => {
            return Err(Error::syntax(SyntaxError::InvalidCharRef {
                reason: format!("invalid char 'U+{u32_value:04x}'"),
            }));
        }
    };

    if !validate::is_xml_char(c) {
        return Err(Error::syntax(SyntaxError::InvalidCharRef {
            reason: format!("invalid XML char '{c}'"),
        }));
    }

    stream.advance(1);

    Ok(c)
}

fn parse_entity_ref<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    stream.expect_byte(b'&')?;
    let name = parse_name(stream)?;
    stream.expect_byte(b';')?;
    Ok(name)
}

// CDSect   ::=  CDStart CData CDEnd
// CDStart  ::=  '<![CDATA['
// CData    ::=  (Char* - (Char* ']]>' Char*))
// CDEnd    ::=  ']]>'
fn parse_cdata<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    parent: Option<usize>,
) -> ParseResult<Node<'a>> {
    stream.advance(9);
    let start = stream.pos;

    loop {
        if stream.is_at_end() {
            return Err(Error::eof());
        }

        if stream.unchecked_current_byte() == b']' && stream.peek_seq("]]>") {
            break;
        }

        stream.advance(1);
    }

    let data = stream.slice(start, stream.pos);
    validate::is_xml_chars(data)?;
    stream.advance(3);

    let id = ctx.current_id();
    let node = Node::new(id, parent, NodeKind::CData(data));

    Ok(node)
}

// CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn parse_text<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    loop {
        let current = stream.current_char()?;
        match current {
            '<' | '&' => break,
            _ => stream.advance(current.len_utf8()),
        }
    }

    let text = stream.slice(start, stream.pos);
    if text.contains("]]>") {
        return Err(Error::syntax(SyntaxError::InvalidCharData));
    }

    Ok(text)
}

fn parse_comment<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
    parent: Option<usize>,
) -> ParseResult<Node<'a>> {
    let start = stream.pos;

    stream.advance(4);
    loop {
        if stream.is_at_end() {
            return Err(Error::eof());
        }

        // For compatibility, the string "--" (double-hyphen) must not occur within comments.
        if stream.unchecked_current_byte() == b'-' && stream.peek_seq("--") {
            if stream.peek_seq("-->") {
                break;
            }

            return Err(Error::syntax(SyntaxError::InvalidComment));
        }

        stream.advance(1);
    }

    stream.advance(3);
    let comment = stream.slice(start, stream.pos);
    validate::is_xml_chars(comment)?;

    let id = ctx.current_id();
    let node = Node::new(id, parent, NodeKind::Comment(comment));

    Ok(node)
}

// children     ::=  (choice | seq) ('?' | '*' | '+')?
// cp           ::=  (Name | choice | seq) ('?' | '*' | '+')?
// choice       ::=  '(' S? cp ( S? '|' S? cp )+ S? ')'         TODO: [VC: Proper Group/PE Nesting]
// seq          ::=  '(' S? cp ( S? ',' S? cp )* S? ')'         TODO: [VC: Proper Group/PE Nesting]
fn parse_element_content_children<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
) -> ParseResult<ElementContentChildren<'a>> {
    enum Type {
        Seq,
        Choice,
    }

    // Leading '(' and any whitespace was already consumed
    let mut content = vec![parse_content_particle(stream, ctx)?];
    let mut content_type: Option<Type> = None;
    let mut expecting_content = false;

    loop {
        stream.consume_whitespace();

        match stream.current_byte()? {
            b'%' => {
                let name = parse_parameter_entity_ref(stream)?;
                content.push(ContentParticle::Name {
                    name,
                    repetition: Repetition::Once,
                });
            }
            b')' => {
                if expecting_content {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "content".into(),
                        actual: ')',
                    }));
                }
                break;
            }
            b',' => {
                if expecting_content {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "content".into(),
                        actual: ',',
                    }));
                }

                match content_type {
                    None => content_type = Some(Type::Seq),
                    Some(Type::Choice) => {
                        return Err(Error::syntax(SyntaxError::InvalidElementContentSeparator {
                            expected: ',',
                            actual: '|',
                        }));
                    }
                    Some(Type::Seq) => {}
                }

                stream.advance(1);
                stream.consume_whitespace();
                expecting_content = true;
            }
            b'|' => {
                if expecting_content {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "content".into(),
                        actual: '|',
                    }));
                }

                match content_type {
                    None => content_type = Some(Type::Choice),
                    Some(Type::Seq) => {
                        return Err(Error::syntax(SyntaxError::InvalidElementContentSeparator {
                            expected: '|',
                            actual: ',',
                        }));
                    }
                    Some(Type::Choice) => {}
                }
                stream.advance(1);
                stream.consume_whitespace();
                expecting_content = true;
            }
            b'(' => {
                if !expecting_content {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "non-content/grouping".into(),
                        actual: '(',
                    }));
                }

                stream.advance(1);
                stream.consume_whitespace();
                let children = parse_element_content_children(stream, ctx)?;
                let repetition = parse_repetition(stream)?;
                let children = match children {
                    ElementContentChildren::Choice(children) => ContentParticle::Choice {
                        content: children,
                        repetition,
                    },
                    ElementContentChildren::Seq(children) => ContentParticle::Seq {
                        content: children,
                        repetition,
                    },
                };
                content.push(children);
                expecting_content = false;
            }
            c => {
                if !expecting_content {
                    return Err(Error::syntax(SyntaxError::UnexpectedCharacter {
                        expected: "non-content".into(),
                        actual: c as char,
                    }));
                }

                let name = parse_name(stream)?;
                let repetition = parse_repetition(stream)?;
                content.push(ContentParticle::Name { name, repetition });
                expecting_content = false;
            }
        }
    }

    stream.expect_byte(b')')?;

    let children = match content_type {
        Some(Type::Choice) => ElementContentChildren::Choice(content),
        Some(Type::Seq) | None => ElementContentChildren::Seq(content),
    };

    Ok(children)
}

fn parse_content_particle<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<ContentParticle<'a>> {
    match stream.current_byte()? {
        b'%' => {
            let name = parse_parameter_entity_ref(stream)?;
            Ok(ContentParticle::Name {
                name,
                repetition: Repetition::Once,
            })
        }
        b'(' => {
            stream.advance(1);
            let children = parse_element_content_children(stream, ctx)?;
            let repetition = parse_repetition(stream)?;
            match children {
                ElementContentChildren::Choice(children) => Ok(ContentParticle::Choice {
                    content: children,
                    repetition,
                }),
                ElementContentChildren::Seq(children) => Ok(ContentParticle::Seq {
                    content: children,
                    repetition,
                }),
            }
        }
        _ => {
            let name = parse_name(stream)?;
            let repetition = parse_repetition(stream)?;
            Ok(ContentParticle::Name { name, repetition })
        }
    }
}

fn parse_repetition<'a>(stream: &mut TokenStream<'a>) -> ParseResult<Repetition> {
    let repetition = match stream.current_byte()? {
        b'?' => {
            stream.advance(1);
            Repetition::QuestionMark
        }
        b'*' => {
            stream.advance(1);
            Repetition::Star
        }
        b'+' => {
            stream.advance(1);
            Repetition::Plus
        }
        _ => Repetition::Once,
    };

    Ok(repetition)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::*;

    fn context() -> Context<'static> {
        Context {
            doc: Document {
                name: None,
                root_node_id: None,
                nodes: Vec::new(),
            },
            prefixes: HashSet::new(),
            entities: HashMap::new(),
            notations: HashMap::new(),
            attr_decls: HashMap::new(),
            element_types: Vec::new(),
            current_node_id: 0,
        }
    }

    fn context_temp_elements() -> Context<'static> {
        Context {
            doc: Document {
                name: None,
                root_node_id: None,
                nodes: Vec::new(),
            },
            prefixes: HashSet::new(),
            entities: HashMap::new(),
            notations: HashMap::new(),
            attr_decls: HashMap::new(),
            element_types: Vec::new(),
            current_node_id: 0,
        }
    }

    fn stream_from(source: &str) -> TokenStream<'_> {
        TokenStream {
            source,
            pos: 0,
            len: source.len(),
        }
    }

    fn stream_with_element(source: &str) -> TokenStream<'_> {
        TokenStream {
            source,
            pos: 0,
            len: source.len(),
        }
    }

    // ========== PI ==========
    #[test]
    fn test_parse_pi() {
        let mut stream = stream_from(r#"<?hello world?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_pi_empty() {
        let mut stream = stream_from(r#"<?hello?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_pi_reserved_target() {
        let mut stream = stream_from(r#"<?xml world?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::UnexpectedDeclaration),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_pi_invalid_target_leading_whitespace() {
        let mut stream = stream_from(r#"<? target world?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::InvalidXmlChar { .. }),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_pi_unexpected_eos() {
        let mut stream = stream_from(r#"<?target"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Eof,
                ..
            })
        ));
    }

    #[test]
    fn test_parse_pi_invalid_name() {
        let mut stream = stream_from("<?L\u{FFFE}L hehe?>");
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_pi_invalid_close_1() {
        let mut stream = stream_from(r#"<?target data?"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Eof,
                ..
            })
        ));
    }

    #[test]
    fn test_parse_pi_invalid_close_2() {
        let mut stream = stream_from(r#"<?target data?<a/>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Eof,
                ..
            })
        ));
    }

    #[test]
    fn test_parse_pi_invalid_data() {
        let mut stream = stream_from("<?target dat\u{FFFF}a?>");
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::InvalidXmlChar { .. }),
                ..
            })
        ));
    }

    // ========== Namespace/QName ==========
    #[test]
    fn test_parse_stag_with_prefix() {
        let res = Parser::parse_from_str(r#"<root xmlns:hello="bob"><hello:world/></root>"#);
        assert!(res.is_ok(),);
    }

    #[test]
    fn test_parse_stag_reserved_prefix() {
        let mut stream = stream_from(r#"<xmlns:world/>"#);
        let mut ctx = context();
        let res = parse_element_start(&mut stream, &mut ctx);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::ReservedPrefix),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_ns_decl() {
        let mut stream = stream_from(r#"xmlns:foo="https://www.google.com""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_ns_decl_default() {
        let mut stream = stream_from(r#"xmlns="https://www.google.com""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_invalid_xml_prefix_uri() {
        let mut stream = stream_with_element(r#"<root xmlns:xml="https://www.google.com"/>"#);
        let mut ctx = context_temp_elements();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Root, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::InvalidXmlPrefixUri),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_unexpected_xml_uri() {
        let mut stream = stream_with_element(r#"<root><foo xmlns:a="http://www.w3.org/XML/1998/namespace"/></root>"#);
        let mut ctx = context_temp_elements();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Root, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::UnexpectedXmlUri),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_unexpected_xmlns_uri() {
        let mut stream = stream_with_element(r#"<root><foo xmlns:a="http://www.w3.org/2000/xmlns/"/><root>"#);
        let mut ctx = context_temp_elements();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Root, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::UnexpectedXmlnsUri),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_unexpected_xmlns_uri_default() {
        let mut stream = stream_with_element(r#"<root xmlns="http://www.w3.org/2000/xmlns/"/>"#);
        let mut ctx = context_temp_elements();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Root, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::UnexpectedXmlnsUri),
                ..
            })
        ));
    }

    // ========== Element ==========
    #[test]
    fn test_parse_stag() {
        let mut stream = stream_from(r#"<hello/>"#);
        let mut ctx = context();
        let res = parse_element_start(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element() {
        let res = Parser::parse_from_str(r#"<root xmlns:ns="ns"><ns:name>hello</ns:name></root>"#);
        assert!(res.is_ok())
    }

    #[test]
    fn test_parse_empty_element() {
        let res = Parser::parse_from_str(r#"<name/>"#);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element_prefix_mismatch() {
        let res = Parser::parse_from_str(r#"<root xmlns:a="a" xmlns:b="b"><a:name></b:name></root>"#);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::ElementNameMismatch { .. }),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_element_name_mismatch() {
        let res = Parser::parse_from_str(r#"<name></notaname>"#);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::ElementNameMismatch { .. }),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_element_invalid_tag_name() {
        let res = Parser::parse_from_str(r#"< name/>"#);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::InvalidXmlName),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_element_namespace_declaration() {
        let mut stream = stream_from(r#"<tag xmlns:foo="http://www.google.com"/>"#);
        let mut ctx = context();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Child, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element_default_namespace_declaration() {
        let mut stream = stream_from(r#"<tag xmlns="http://www.google.com"/>"#);
        let mut ctx = context();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Child, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element_with_attributes() {
        let mut stream = stream_from(r#"<tag some="value" another="one"></tag>"#);
        let mut ctx = context();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Child, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element_empty_with_attributes() {
        let mut stream = stream_from(r#"<name some="value" another="one"/>"#);
        let mut ctx = context();
        let res = parse_element(&mut stream, &mut ctx, ElementType::Child, None);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_attribute() {
        let mut stream = stream_from(r#"b="c""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_element_duplicate_attribute() {
        let res = Parser::parse_from_str(r#"<name some="value" some="value"/>"#);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::DuplicateAttribute { .. }),
                ..
            })
        ));
    }

    #[test]
    fn test_parse_char_ref_decimal() {
        let mut stream = stream_from(r#"&#85;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_ok())
    }

    #[test]
    fn test_parse_char_ref_invalid_decimal_digit() {
        let mut stream = stream_from(r#"&#99a;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_char_ref_invalid_decimal_out_of_bounds() {
        // 0x110000
        let mut stream = stream_from(r#"&#1114112;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_char_ref_hexadecimal() {
        let mut stream = stream_from(r#"&#x2f;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_ok())
    }

    #[test]
    fn test_parse_char_ref_invalid_hexadecimal_digit() {
        let mut stream = stream_from(r#"&#x11M22;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_char_ref_invalid_hexadecimal_out_of_bounds() {
        let mut stream = stream_from(r#"&#x110100;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_char_ref_invalid_hexadecimal_invalid_xml_char() {
        let mut stream = stream_from(r#"&#xfffe;"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_char_ref_unclosed() {
        let mut stream = stream_from(r#"&#x22f"#);
        let res = parse_character_reference(&mut stream);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_system_only() {
        let mut stream = stream_from(r#"<!NOTATION gif SYSTEM "hello.gif">"#);
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_notation_decl_public_only() {
        let mut stream = stream_from(r#"<!NOTATION gif PUBLIC 'hello.gif'>"#);
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_notation_decl_public_system() {
        let mut stream = stream_from(r#"<!NOTATION gif PUBLIC 'hello.gif' "sure why not">"#);
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_ok())
    }

    #[test]
    fn test_parse_notation_decl_unclosed_quote() {
        let mut stream = stream_from(r#"<!NOTATION gif SYSTEM 'hello.gif">"#);
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_public_system_missing_space() {
        let mut stream = stream_from(r#"<!NOTATION gif PUBLIC 'hello.gif'"sure why not">"#);
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_extra_whitespace() {
        let mut stream = stream_from("<!NOTATION  \t  gif \nSYSTEM \"hello.gif\"       >");
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_ok())
    }

    #[test]
    fn test_parse_notation_decl_missing_whitespace_after_decl() {
        let mut stream = stream_from("<!NOTATIONgif SYSTEM \"hello.gif\">");
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_missing_whitespace_after_name() {
        let mut stream = stream_from("<!NOTATION gifSYSTEM\"hello.gif\">");
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_missing_external_or_public_id() {
        let mut stream = stream_from("<!NOTATION gif \"hello.gif\">");
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_decl_missing_external_or_public_id_2() {
        let mut stream = stream_from("<!NOTATION gif NOTSYSTEM \"hello.gif\">");
        let mut ctx = context();
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_notation_duplicate_decl() {
        let mut stream = stream_from("<!NOTATION gif NOTSYSTEM \"hello.gif\">");
        let mut ctx = context();
        ctx.notations.insert(
            "gif",
            Notation {
                system_id: Some("hello.gif"),
                public_id: None,
            },
        );
        let res = parse_notation_decl(&mut stream, &mut ctx);
        assert!(res.is_err())
    }

    #[test]
    fn test_parse_comment_empty() {
        let mut stream = stream_from("<!---->");
        let mut ctx = context();
        let res = parse_comment(&mut stream, &mut ctx, None);
        assert!(res.is_ok())
    }

    #[test]
    fn test_comment_invalid_double_hyphen() {
        let mut stream = stream_from("<!----->");
        let mut ctx = context();
        let res = parse_comment(&mut stream, &mut ctx, None);
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::Syntax(SyntaxError::InvalidComment),
                ..
            })
        ));
    }

    #[test]
    fn test_expand_entity_ref_single() {
        let mut ctx = context();
        let mut stream = stream_from("Hello, &world;!");
        let entity_ref = "Hello, &world;!";
        ctx.entities.insert(
            "world",
            Entity::new("world", EntityType::InternalGeneral { value: "Mary Sue" }),
        );

        let expanded = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false).unwrap();
        let res = String::from_utf8_lossy(&expanded);
        assert_eq!("Hello, Mary Sue!", res);
    }

    #[test]
    fn test_expand_entity_ref_nested() {
        let mut ctx = context();
        let mut stream = stream_from("Hello, &world;!");
        let entity_ref = "Hello, &world;";
        ctx.entities
            .insert("a", Entity::new("a", EntityType::InternalGeneral { value: "rld!" }));
        ctx.entities
            .insert("b", Entity::new("b", EntityType::InternalGeneral { value: "Wo&a;" }));
        ctx.entities.insert(
            "world",
            Entity::new("world", EntityType::InternalGeneral { value: "&b;" }),
        );

        let expanded = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false).unwrap();
        let res = String::from_utf8_lossy(&expanded);
        assert_eq!("Hello, World!", res);
    }

    #[test]
    fn test_expand_entity_ref_multiple() {
        let mut ctx = context();
        let mut stream = stream_from("one &two; three &four; &five;");
        let entity_ref = "one &two; three &four; &five;";
        ctx.entities
            .insert("two", Entity::new("two", EntityType::InternalGeneral { value: "two" }));
        ctx.entities.insert(
            "four",
            Entity::new("four", EntityType::InternalGeneral { value: "four" }),
        );
        ctx.entities.insert(
            "five",
            Entity::new("five", EntityType::InternalGeneral { value: "five" }),
        );

        let expanded = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false).unwrap();
        let res = String::from_utf8_lossy(&expanded);
        assert_eq!("one two three four five", res);
    }

    #[test]
    fn test_expand_entity_ref_empty() {
        let mut ctx = context();
        let mut stream = stream_from("ab&;cd");
        let entity_ref = "ab&;cd";

        let res = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false);
        assert!(res.is_err());
    }

    #[test]
    fn test_expand_entity_ref_unclosed() {
        let mut ctx = context();
        let mut stream = stream_from("ab&cd");
        let entity_ref = "ab&cd";

        let res = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false);
        assert!(res.is_err());
    }

    #[test]
    fn test_expand_entity_ref_illegal_char() {
        let mut ctx = context();
        let mut stream = stream_from("ab&wh&at;cd");
        let entity_ref = "ab&wh&at;cd";

        let res = expand_entity_ref(&mut stream, &mut ctx, entity_ref, false);
        assert!(res.is_err());
    }
}
