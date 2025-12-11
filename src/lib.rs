type ParseResult<T> = std::result::Result<T, ParseError>;

mod validate;

const XML_PREFIX: &str = "xml";
const XML_URI: &str = "http://www.w3.org/XML/1998/namespace";
const XMLNS_PREFIX: &str = "xmlns";
const XMLNS_URI: &str = "http://www.w3.org/2000/xmlns/";

#[derive(Debug, PartialEq)]
pub enum ParseError {
    /// Duplicate Attribute
    DuplicateAttribute(String, Span),

    /// Duplicate Element Type
    ///
    /// An element type MUST NOT be declared more than once.
    DuplicateElementType(String, Span),

    /// Duplicate Mixed Content name
    ///
    /// The same name MUST NOT appear more than once in a single mixed-content declaration.
    DuplicateMixedContent(String, Span),

    /// Invalid Character Data
    ///
    /// Unescaped ampersands (`&`) and left angle brackets (`<`) will be parsed as separate errors.
    /// This error represents a [`CharData`] that contains the CDATA-section-close delimiter `]]>`
    InvalidCharData(Span),

    /// Invalid Comment
    InvalidComment(String, Span),

    /// Invalid Declaration
    ///
    /// Missing `version` or unclosed declaration
    InvalidDeclaration(String, Span),

    /// Invalid Element Type declaration
    ///
    /// Well formed Element Type declarations must contain a valid name and [`Content Spec`]
    InvalidElementTypeDecl(Span),

    /// Invalid Namespace character
    InvalidNamespaceChar(char, Span),

    /// Invalid Root Name
    ///
    /// If a `DTD` is present, the root element name must match the `DOCTYPE`.
    ///
    /// Expected, actual, span
    InvalidRootName(String, String, Span),

    /// Invalid Standalone
    ///
    /// Valid values [yes | no]
    InvalidStandalone(String, Span),

    /// Invalid XML character
    InvalidXmlChar(char, Span),

    /// Invalid XML Name
    InvalidXmlName(Span),

    /// Invalid XML Prefix URI
    ///
    /// The `xml` prefix must be bound to the `http://www.w3.org/XML/1998/namespace` namespace name
    InvalidXmlPrefixUri(Span),

    /// Missing Root Node
    ///
    /// A Document must have at least one element
    MissingRoot,

    /// Reserved Namespace
    ///
    /// `xmlns` is a reserved Namespace and must not be used
    ReservedPrefix(Span),

    /// Tag Name Mismatch
    ///
    /// Start and End tag of an Element must have the same prefix:local
    /// expected, actual, span
    TagNameMismatch(String, String, Span),

    /// Root Node was not properly closed
    UnclosedRoot,

    /// Unexpected XML Declaration encountered.
    ///
    /// If present, the Declaration must be at the start of the Document
    UnexpectedDeclaration(Span),

    /// Unexpected character
    ///
    /// expected, actual, span
    UnexpectedCharacter(u8, u8, Span),

    /// Like `UnexpectedCharacter`, only there is more than one
    /// valid expected character.
    ///
    /// expected, actual, span
    UnexpectedCharacter2(&'static str, u8, Span),

    /// Unexpected Element
    ///
    /// An element besides a comment or PI was encountered after
    /// the root element
    UnexpectedElement(Span),

    /// The input stream ended unexpectedly.
    UnexpectedEndOfStream,

    /// Unexpected XML URI
    ///
    /// Prefixes other than `xml` MUST NOT be bound to the `http://www.w3.org/XML/1998/namespace`
    /// namespace name
    UnexpectedXmlUri(Span),

    /// Unexpected XMLNS URI
    ///
    /// The URI `http://www.w3.org/2000/xmlns/` is bound to the `xmlns` prefix and MUST NOT be declared
    UnexpectedXmlnsUri(Span),

    /// Unknown Prefix
    ///
    /// An Namespace prefix was used that was not previously declared or in scope
    UnknownPrefix(String, Span),

    /// wut
    WTF(Span),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::DuplicateAttribute(attribute, span) => {
                writeln!(
                    f,
                    "error:{}:{}: duplicate attribute '{}'",
                    span.row, span.col_start, attribute
                )?;
                writeln!(f, "{span}")
            }
            ParseError::DuplicateElementType(element, span) => {
                writeln!(
                    f,
                    "error:{}:{}: duplicate element type declaration '{}'",
                    span.row, span.col_start, element
                )?;
                writeln!(f, "{span}")
            }
            ParseError::DuplicateMixedContent(element, span) => {
                writeln!(
                    f,
                    "error:{}:{}: duplicate name '{}' in mixed-content declaration",
                    span.row, span.col_start, element
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidCharData(span) => {
                writeln!(
                    f,
                    "error:{}:{}: sequence ']]>' is not allowed inside character data",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidComment(reason, span) => {
                writeln!(f, "error:{}:{}: invalid comment: {}", span.row, span.col_start, reason)?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidDeclaration(expected, span) => {
                writeln!(
                    f,
                    "error:{}:{}: invalid declaration: expected [{}]",
                    span.row, span.col_start, expected
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidElementTypeDecl(span) => {
                writeln!(
                    f,
                    "error:{}:{}: malformed element type declaration",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidNamespaceChar(c, span) => {
                writeln!(
                    f,
                    "error:{}:{}: invalid namespace character encountered {:?}",
                    span.row, span.col_start, c
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidRootName(expected, actual, span) => {
                writeln!(
                    f,
                    "error{}:{}: Document root element \"{actual}\" must match DOCTYPE \"{expected}\"",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidStandalone(actual, span) => {
                writeln!(f, "error{}:{}: invalid standalone value", span.row, span.col_start)?;
                writeln!(f, "  Expected: [yes | no]\n  Actual:   [{actual}]")?;
                writeln!(f, "\n{span}")
            }
            ParseError::InvalidXmlName(span) => {
                writeln!(f, "error:{}:{}: invalid tag", span.row, span.col_start)?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidXmlChar(c, span) => {
                writeln!(
                    f,
                    "error:{}:{}: invalid XML character encountered {:?}",
                    span.row, span.col_start, c
                )?;
                writeln!(f, "{span}")
            }
            ParseError::InvalidXmlPrefixUri(span) => {
                writeln!(
                    f,
                    "error:{}:{}: invalid namespace bound to 'xml' prefix",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::MissingRoot => {
                writeln!(f, "error: no root element found")
            }
            ParseError::ReservedPrefix(span) => {
                writeln!(
                    f,
                    "error:{}:{}: 'xmlns' is a reserved prefix and must not be used",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::TagNameMismatch(expected, actual, span) => {
                writeln!(f, "error:{}:{}: unexpected tag name", span.row, span.col_start)?;
                writeln!(f, "  Expected: '{expected}'\n  Actual:   '{actual}'")?;
                write!(f, "{span}")
            }
            ParseError::UnclosedRoot => {
                writeln!(f, "error: unclosed root element")
            }
            ParseError::UnexpectedDeclaration(span) => {
                writeln!(
                    f,
                    "error:{}:{}: unexpected XML declaration\n{}",
                    span.row, span.col_start, span
                )
            }
            ParseError::UnexpectedCharacter(expected, actual, span) => {
                writeln!(f, "error:{}:{}: unexpected character at", span.row, span.col_start)?;
                writeln!(
                    f,
                    "  Expected: '{}'\n  Actual:   '{}'",
                    *expected as char, *actual as char
                )?;
                write!(f, "{span}")
            }
            ParseError::UnexpectedCharacter2(expected, actual, span) => {
                writeln!(f, "error:{}:{}: unexpected character", span.row, span.col_start)?;
                writeln!(f, "  expected: {}\n  actual:   '{}'", expected, *actual as char)?;
                writeln!(f, "{span}")
            }
            ParseError::UnexpectedElement(span) => {
                writeln!(f, "error:{}:{}: unexpected element", span.row, span.col_start)?;
                writeln!(f, "{span}")
            }
            ParseError::UnexpectedXmlUri(span) => {
                writeln!(
                    f,
                    "error:{}:{}: 'xml' namespace URI bound to non 'xml' prefix",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::UnexpectedXmlnsUri(span) => {
                writeln!(
                    f,
                    "error:{}:{}: `xmlns` URI must not be declared",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
            ParseError::UnknownPrefix(prefix, span) => {
                writeln!(
                    f,
                    "error:{}:{}: unknown namespace prefix `{}`",
                    span.row, span.col_start, prefix,
                )?;
                writeln!(f, "{span}")
            }
            ParseError::UnexpectedEndOfStream => write!(f, "error: unexpected end of stream"),
            ParseError::WTF(span) => {
                write!(
                    f,
                    "error:{}:{}: something unexpected happened",
                    span.row, span.col_start
                )?;
                writeln!(f, "{span}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Span {
    row: usize,
    col_start: usize,
    col_end: usize,
    line: String,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let row_offset = (self.row.checked_ilog10().unwrap_or(0) + 1) as usize;
        let col_offset = self.col_start - 1;
        let err_len = self.col_end - self.col_start;

        writeln!(f, " {:>row_offset$} | ", "")?;
        writeln!(f, " {:>row_offset$} | {}", self.row, self.line)?;
        writeln!(f, " {:>row_offset$} | {:>col_offset$}{:^>err_len$}", "", "", "^")?;
        write!(f, " {:>row_offset$} | ", "")
    }
}

impl Span {
    fn new(source: &str, start: usize, end: usize) -> Self {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in source.chars().enumerate() {
            if i == start {
                break;
            }

            if c == '\n' {
                col = 1;
                row += 1;
                continue;
            }

            col += 1;
        }

        let line = source.split('\n').nth(row - 1).unwrap_or_default().to_string();

        Self {
            row,
            col_start: col,
            col_end: col + (end - start),
            line,
        }
    }
}

#[derive(Debug, Default)]
pub struct Document<'a> {
    pub name: Option<&'a str>,
    pub root_node_id: Option<usize>,
    pub nodes: Vec<Node<'a>>,
    pub namespaces: Vec<Namespace<'a>>,
}

impl<'a> Document<'a> {
    pub fn get_elements_by_name(&'a self, tag_name: &'a str) -> Vec<&'a Node<'a>> {
        fn walk<'a>(nodes: &'a Vec<Node<'a>>, tag_name: &'a str) -> Vec<&'a Node<'a>> {
            let mut elements = Vec::new();

            for node in nodes {
                if let NodeKind::Element { name, children, .. } = &node.data {
                    if *name == tag_name {
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

    /// Returns an owned list of valid Namespaces for a given `node_id`
    fn namespaces_in_scope(&self, node_id: usize, current_node_id: usize) -> Vec<Namespace<'a>> {
        self.namespaces
            .iter()
            .filter(|ns| {
                // Namespace was declared before first use
                node_id >= ns.start &&
                // Namespace is still in scope
                (ns.end == Some(current_node_id) || ns.end.is_none())
            })
            .cloned()
            .collect()
    }
}

#[derive(Debug, PartialEq)]
struct TempElementData<'a> {
    prefix: Option<&'a str>,
    local: &'a str,
    namespaces: Vec<Namespace<'a>>,
    children: Vec<Node<'a>>,
    attributes: Vec<Attribute<'a>>,
    id: usize,
    parent_id: Option<usize>,
}

#[derive(PartialEq)]
pub struct Attribute<'a> {
    pub name: &'a str,
    pub value: &'a str,
}

impl std::fmt::Debug for Attribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Attribute {{ name: {}, value: {} }}", self.name, self.value)
    }
}

#[derive(Clone, PartialEq)]
pub struct Namespace<'a> {
    pub name: Option<&'a str>,
    pub uri: &'a str,
    start: usize,
    end: Option<usize>,
}

impl std::fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Namespace {{ name: {:?}, uri: {} }}", self.name, self.uri,)
    }
}

#[derive(PartialEq)]
pub struct Node<'a> {
    pub id: usize,
    parent_id: Option<usize>,
    pub data: NodeKind<'a>,
}

impl std::fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]{:#?}", self.id, self.data)
    }
}

#[derive(Debug, PartialEq)]
pub enum NodeKind<'a> {
    Declaration {
        version: &'a str,
        encoding: Option<&'a str>,
        standalone: Option<&'a str>,
    },
    Element {
        name: &'a str,
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

#[derive(Debug)]
struct Entity<'a> {
    name: &'a str,
    value: &'a str,
}

#[derive(Debug)]
struct ElementTypeDecl<'a> {
    name: &'a str,
    content_spec: ContentSpec<'a>,
}

#[derive(Debug)]
enum ContentSpec<'a> {
    Empty,
    Any,
    MixedContent(Vec<&'a str>),
    ElementContent(Vec<ElementContent<'a>>),
}

#[derive(Debug)]
enum ElementContent<'a> {
    Name { name: &'a str, repetition: Repetition },
    Choice(Vec<ElementContent<'a>>),
    Seq(Vec<ElementContent<'a>>),
}

#[derive(Debug)]
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
            return Err(ParseError::UnexpectedEndOfStream);
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
            return Err(ParseError::UnexpectedEndOfStream);
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
        char::from_u32(res).ok_or(ParseError::WTF(self.span_single()))
    }

    fn unchecked_current_byte(&mut self) -> u8 {
        self.source.as_bytes()[self.pos]
    }

    fn peek_byte(&mut self) -> ParseResult<u8> {
        if self.pos + 1 >= self.len {
            return Err(ParseError::UnexpectedEndOfStream);
        }

        Ok(self.source.as_bytes()[self.pos + 1])
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

    fn span(&self, start: usize, end: usize) -> Span {
        Span::new(self.source, start, end)
    }

    fn span_single(&self) -> Span {
        Span::new(self.source, self.pos, self.pos + 1)
    }

    fn is_white_space(&mut self) -> ParseResult<bool> {
        let current = self.current_byte()?;
        Ok(matches!(current, 0x20 | 0x9 | 0xD | 0xA))
    }

    fn expect_and_consume_whitespace(&mut self) -> ParseResult<()> {
        if !self.is_white_space()? {
            return Err(ParseError::UnexpectedCharacter2(
                "whitespace",
                self.unchecked_current_byte(),
                self.span_single(),
            ));
        }

        self.consume_whitespace();

        Ok(())
    }

    fn expect_byte(&mut self, c: u8) -> ParseResult<()> {
        let current = self.current_byte()?;
        if current != c {
            return Err(ParseError::UnexpectedCharacter(c, current, self.span_single()));
        }

        self.advance(1);
        Ok(())
    }

    fn consume_whitespace(&mut self) {
        while !self.is_at_end() {
            // We know we aren't at the end so this will not panic
            if !self.is_white_space().expect("unexpected EOF") {
                break;
            }

            self.advance(1);
        }
    }

    fn consume_quote(&mut self) -> ParseResult<u8> {
        let current = self.current_byte()?;
        if current == b'\'' || current == b'"' {
            self.advance(1);
            return Ok(current);
        }

        Err(ParseError::UnexpectedCharacter2("a quote", current, self.span_single()))
    }

    fn has_prefix(&self, ctx: &mut Context<'a>, prefix: &str) -> bool {
        if prefix == XMLNS_PREFIX {
            return true;
        }

        let mut parent_id: Option<usize> = None;
        let mut elements = ctx.temp_elements.iter().rev();

        // Check first element to get an initial parent index, if needed.
        if let Some(current) = elements.next() {
            if current.namespaces.iter().any(|ns| ns.name == Some(prefix)) {
                return true;
            }
            parent_id = current.parent_id;
        }

        for parent in elements {
            // Skip siblings
            if parent_id != Some(parent.id) {
                continue;
            }

            if parent.namespaces.iter().any(|ns| ns.name == Some(prefix)) {
                return true;
            }

            // Reached root node without finding prefix declaration
            if parent.parent_id.is_none() {
                return false;
            }

            parent_id = parent.parent_id;
        }

        false
    }
}

pub struct Context<'a> {
    doc: Document<'a>,
    temp_elements: Vec<TempElementData<'a>>,
    entities: Vec<Entity<'a>>,
    element_types: Vec<ElementTypeDecl<'a>>,
    current_node_id: usize,
}

impl<'a> Context<'a> {
    fn emit_token(&mut self, stream: &mut TokenStream<'a>, token: Token<'a>) -> ParseResult<()> {
        match token {
            Token::ElementStart { prefix, local } => {
                let parent_id = self.temp_elements.last().map(|parent| parent.id);
                self.temp_elements.push(TempElementData {
                    prefix,
                    local,
                    namespaces: Vec::new(),
                    children: Vec::new(),
                    attributes: Vec::new(),
                    id: self.current_node_id,
                    parent_id,
                });
                self.current_node_id += 1;
            }
            Token::Text { text } => {
                if let Some(mut parent) = self.temp_elements.pop() {
                    let node = Node {
                        id: self.current_node_id,
                        parent_id: Some(parent.id),
                        data: NodeKind::Text(text),
                    };
                    self.current_node_id += 1;
                    parent.children.push(node);
                    self.temp_elements.push(parent);
                }
            }
            Token::ElementEnd { kind } => match kind {
                ElementEndKind::Open => {}
                ElementEndKind::Empty => {
                    if let Some(current) = self.temp_elements.pop() {
                        for ns in &mut self.doc.namespaces {
                            if ns.start == current.id {
                                ns.end = Some(self.current_node_id);
                            }
                        }

                        let namespaces = self.doc.namespaces_in_scope(current.id, self.current_node_id);
                        let mut node = Node {
                            id: current.id,
                            parent_id: None,
                            data: NodeKind::Element {
                                name: current.local,
                                namespaces,
                                attributes: current.attributes,
                                children: current.children,
                            },
                        };

                        if let Some(mut parent) = self.temp_elements.pop() {
                            node.parent_id = Some(parent.id);
                            parent.children.push(node);
                            self.temp_elements.push(parent);
                        }
                        // root node
                        else {
                            self.doc.nodes.push(node);
                        }
                    } else {
                        // FIXME: please
                        panic!()
                    }
                }
                ElementEndKind::Close { prefix, local } => {
                    if let Some(start) = self.temp_elements.pop() {
                        if start.prefix != prefix || start.local != local {
                            let (expected, actual) = match start.prefix {
                                Some(prefix) => (format!("{}:{}", prefix, start.local), format!("{prefix}:{local}")),
                                None => (start.local.to_string(), local.to_string()),
                            };
                            let start = stream.pos - actual.len() - 1;
                            let span = stream.span(start, stream.pos - 1);
                            return Err(ParseError::TagNameMismatch(expected, actual, span));
                        }

                        for ns in &mut self.doc.namespaces {
                            if ns.start == start.id {
                                ns.end = Some(self.current_node_id);
                            }
                        }

                        let namespaces = self.doc.namespaces_in_scope(start.id, self.current_node_id);
                        let mut node = Node {
                            id: start.id,
                            parent_id: None,
                            data: NodeKind::Element {
                                name: start.local,
                                namespaces,
                                attributes: start.attributes,
                                children: start.children,
                            },
                        };

                        if let Some(mut parent) = self.temp_elements.pop() {
                            node.parent_id = Some(parent.id);
                            parent.children.push(node);
                            self.temp_elements.push(parent);
                        }
                        // root node
                        else {
                            self.doc.root_node_id = Some(node.id);
                            self.doc.nodes.push(node);
                        }
                    }
                }
            },
            Token::Attribute { prefix, local, value } => {
                if let Some(current) = self.temp_elements.last_mut() {
                    match prefix {
                        Some(prefix) => {
                            // prefixed namespace - 'xmlns:prefix="foobarbaz"'
                            if prefix == XMLNS_PREFIX {
                                // 'xml'
                                if local == XML_PREFIX {
                                    // 'xml' prefix can only be bound to the 'http://www.w3.org/XML/1998/namespace' namespace
                                    if value != XML_URI {
                                        let start = stream.pos - value.len() - 1;
                                        return Err(ParseError::InvalidXmlPrefixUri(
                                            stream.span(start, stream.pos - 1),
                                        ));
                                    }
                                }

                                // Prefixes other than `xml` MUST NOT be bound to the `http://www.w3.org/XML/1998/namespace` namespace name
                                if value == XML_URI {
                                    let start = stream.pos - prefix.len() - local.len() - value.len() - 4;
                                    return Err(ParseError::UnexpectedXmlUri(stream.span(start, stream.pos - 1)));
                                }

                                // The 'xmlns' prefix is bound to the 'http://www.w3.org/2000/xmlns/' namespace and MUST NOT be declared
                                if value == XMLNS_URI {
                                    let start = stream.pos - prefix.len() - local.len() - value.len() - 4;
                                    return Err(ParseError::UnexpectedXmlnsUri(stream.span(start, stream.pos - 1)));
                                }

                                let ns = Namespace {
                                    name: Some(local),
                                    uri: value,
                                    start: current.id,
                                    end: None,
                                };
                                if !self.doc.namespaces.contains(&ns) {
                                    self.doc.namespaces.push(ns.clone());
                                }
                                if !current.namespaces.contains(&ns) {
                                    current.namespaces.push(ns);
                                }
                            }
                        }
                        None => {
                            // Default/unprefixed namespace - 'xmlns="foobarbaz"'
                            if local == XMLNS_PREFIX {
                                // The 'xmlns' prefix is bound to the 'http://www.w3.org/2000/xmlns/' namespace and MUST NOT be declared
                                if value == XMLNS_URI {
                                    let start = stream.pos - local.len() - value.len() - 3;
                                    return Err(ParseError::UnexpectedXmlnsUri(stream.span(start, stream.pos - 1)));
                                }

                                let ns = Namespace {
                                    name: None,
                                    uri: value,
                                    start: current.id,
                                    end: None,
                                };
                                self.doc.namespaces.push(ns.clone());
                                current.namespaces.push(ns);
                            }
                            // Attribute
                            else {
                                if current.attributes.iter().any(|a| a.name == local) {
                                    let start = stream.pos - local.len() - value.len() - 3;
                                    return Err(ParseError::DuplicateAttribute(
                                        local.to_string(),
                                        stream.span(start, stream.pos),
                                    ));
                                }

                                current.attributes.push(Attribute { name: local, value });
                            }
                        }
                    }
                }
            }
            Token::ProcessingInstruction { target, data } => {
                let mut node = Node {
                    id: self.current_node_id,
                    parent_id: None,
                    data: NodeKind::ProcessingInstruction { target, data },
                };
                self.current_node_id += 1;
                match self.temp_elements.last_mut() {
                    Some(parent) => {
                        node.parent_id = Some(parent.id);
                        parent.children.push(node);
                    }
                    None => self.doc.nodes.push(node),
                }
            }
            Token::Declaration {
                version,
                encoding,
                standalone,
            } => {
                let node = Node {
                    id: self.current_node_id,
                    parent_id: None,
                    data: NodeKind::Declaration {
                        version,
                        encoding,
                        standalone,
                    },
                };
                self.current_node_id += 1;
                self.doc.nodes.push(node);
            }
            Token::Comment { comment } => {
                let mut node = Node {
                    id: self.current_node_id,
                    parent_id: None,
                    data: NodeKind::Comment(comment),
                };
                self.current_node_id += 1;
                match self.temp_elements.last_mut() {
                    Some(parent) => {
                        node.parent_id = Some(parent.id);
                        parent.children.push(node);
                    }
                    None => self.doc.nodes.push(node),
                }
            }
            Token::CData { data } => {
                let mut node = Node {
                    id: self.current_node_id,
                    parent_id: None,
                    data: NodeKind::CData(data),
                };
                self.current_node_id += 1;
                match self.temp_elements.last_mut() {
                    Some(parent) => {
                        node.parent_id = Some(parent.id);
                        parent.children.push(node);
                    }
                    None => self.doc.nodes.push(node),
                }
            }
        }

        Ok(())
    }
}

pub struct Parser;

impl Parser {
    pub fn parse(source: &str) -> ParseResult<Document<'_>> {
        let doc = Document {
            name: None,
            root_node_id: None,
            nodes: Vec::new(),
            namespaces: Vec::new(),
        };

        let mut ctx = Context {
            doc,
            temp_elements: Vec::new(),
            entities: Vec::new(),
            element_types: Vec::new(),
            current_node_id: 0,
        };

        let mut stream = TokenStream {
            source,
            len: source.len(),
            pos: 0,
        };

        parse_document(&mut stream, &mut ctx)?;

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
            b'<' if stream.starts_with("<?") => parse_processing_instruction(stream, ctx)?,
            b'<' if stream.starts_with("<!-- ") => parse_comment(stream, ctx)?,
            // Start of the root node, break and parse outside of the loop.
            b'<' => break,
            _ => return Err(ParseError::WTF(stream.span_single())),
        }
    }

    stream.consume_whitespace();

    if stream.is_at_end() {
        return Err(ParseError::MissingRoot);
    }

    parse_element(stream, ctx, ElementType::Root)?;

    if !ctx.temp_elements.is_empty() {
        return Err(ParseError::UnclosedRoot);
    }

    // Parse any comments or PIs after the root node
    parse_misc(stream, ctx)?;

    if !stream.is_at_end() {
        return Err(ParseError::UnexpectedElement(stream.span(stream.pos, stream.pos + 1)));
    }

    Ok(())
}

fn parse_name<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    let current = stream.current_char()?;
    if !validate::is_name_start_char(current) {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }

    stream.advance(current.len_utf8());

    loop {
        let c = stream.current_char()?;
        if !validate::is_name_char(c) {
            break;
        }
        stream.advance(c.len_utf8());
    }

    let name = stream.slice(start, stream.pos);
    validate::is_valid_name(name, start, stream)?;

    Ok(name)
}

fn parse_nc_name<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let start = stream.pos;

    let current = stream.current_char()?;
    if !validate::is_name_start_char(current) || current == ':' {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }
    stream.advance(current.len_utf8());

    loop {
        let c = stream.current_char()?;
        if !validate::is_name_char(c) || c == ':' {
            break;
        }
        stream.advance(c.len_utf8());
    }

    let name = stream.slice(start, stream.pos);
    validate::is_valid_name(name, start, stream)?;

    Ok(name)
}

// prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if stream.starts_with("<?xml ") {
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
            b'<' if stream.starts_with("<?") => parse_processing_instruction(stream, ctx)?,
            b'<' if stream.starts_with("<!-- ") => parse_comment(stream, ctx)?,
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
// SDDecl        ::=   S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"')) [VC: Standalone Document Declaration]
fn parse_xml_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(5);
    stream.consume_whitespace();

    if !stream.peek_seq("version") {
        return Err(ParseError::InvalidDeclaration(
            String::from("version attribute"),
            stream.span(stream.pos, stream.pos + 1),
        ));
    }

    stream.advance(7);
    stream.consume_whitespace();
    stream.expect_byte(b'=')?;
    stream.consume_whitespace();

    let version = parse_attribute_value(stream)?;

    stream.consume_whitespace();
    let encoding = if stream.starts_with("encoding") {
        stream.advance(8);
        stream.expect_byte(b'=')?;
        Some(parse_attribute_value(stream)?)
    } else {
        None
    };

    stream.consume_whitespace();
    let standalone = if stream.peek_seq("standalone") {
        let start = stream.pos;
        stream.advance(10);
        stream.expect_byte(b'=')?;

        let value = parse_attribute_value(stream)?;
        if value == "yes" || value == "no" {
            Some(value)
        } else {
            return Err(ParseError::InvalidStandalone(
                value.to_owned(),
                stream.span(start, stream.pos),
            ));
        }
    } else {
        None
    };

    // TODO: While it is valid for the declaration to span multiple lines, if there is no '?>'
    // and the next non-whitespace character is the next element, the diagnostics will report
    // the incorrect source line.
    stream.consume_whitespace();
    if !stream.peek_seq("?>") {
        return Err(ParseError::InvalidDeclaration(
            String::from("?>"),
            stream.span(stream.pos, stream.pos + 1),
        ));
    }

    stream.advance(2);
    ctx.emit_token(
        stream,
        Token::Declaration {
            version,
            encoding,
            standalone,
        },
    )?;

    Ok(())
}

// PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
// PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    if stream.starts_with("<?xml ") {
        return Err(ParseError::UnexpectedDeclaration(
            stream.span(stream.pos, stream.pos + 1),
        ));
    }

    stream.advance(2);
    let target = parse_name(stream)?;

    if let Ok(b'?') = stream.current_byte() {
        let peek = stream.peek_byte()?;
        match peek {
            b'>' => {
                stream.advance(2);
                ctx.emit_token(stream, Token::ProcessingInstruction { target, data: None })?;
                return Ok(());
            }
            c => return Err(ParseError::UnexpectedCharacter(b'>', c, stream.span_single())),
        }
    }

    stream.consume_whitespace();
    let data_start = stream.pos;
    loop {
        if stream.is_at_end() {
            return Err(ParseError::UnexpectedEndOfStream);
        }

        if let Ok(b'?') = stream.current_byte() {
            let peek = stream.peek_byte()?;
            match peek {
                b'>' => {
                    break;
                }
                c => return Err(ParseError::UnexpectedCharacter(b'>', c, stream.span_single())),
            }
        }

        stream.advance(1);
    }

    let data = stream.slice(data_start, stream.pos);
    validate::is_xml_chars(data, data_start, stream)?;

    stream.advance(2);
    ctx.emit_token(
        stream,
        Token::ProcessingInstruction {
            target,
            data: Some(data),
        },
    )?;

    Ok(())
}

// doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
// DeclSep     ::=   PEReference | S [WFC: PE Between Declarations]
// intSubset   ::=   (markupdecl | DeclSep)*
// markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn parse_doc_type_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);
    stream.expect_and_consume_whitespace()?;

    let name = parse_name(stream)?;
    ctx.doc.name = Some(name);

    // TODO: ExternalId

    stream.consume_whitespace();

    if let Ok(b'[') = stream.current_byte() {
        stream.expect_byte(b'[')?;

        // Parse 'intSubset'
        loop {
            // TODO:
            // - elementdecl
            // - AttlistDecl
            // - NotationDecl
            match stream.current_byte()? {
                b']' => break,
                b'<' if stream.starts_with("<!ELEMENT") => parse_element_type_decl(stream, ctx)?,
                b'<' if stream.starts_with("<!ENTITY") => parse_entity_decl(stream, ctx)?,
                b'<' if stream.starts_with("<?") => parse_processing_instruction(stream, ctx)?,
                b'<' if stream.starts_with("<!-- ") => parse_comment(stream, ctx)?,
                _ if stream.is_white_space()? => stream.advance(1),
                _ => todo!(),
            }
        }

        stream.expect_byte(b']')?;
        stream.consume_whitespace();
    }

    stream.expect_byte(b'>')?;

    Ok(())
}

// elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
// contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);

    stream.expect_and_consume_whitespace()?;
    let start = stream.pos;
    let name = parse_name(stream)?;

    if stream.expect_and_consume_whitespace().is_err() {
        return Err(ParseError::InvalidElementTypeDecl(stream.span(start + 1, stream.pos)));
    }

    // An element type MUST NOT be declared more than once.
    if ctx.element_types.iter().any(|el| el.name == name) {
        return Err(ParseError::DuplicateElementType(
            name.to_string(),
            stream.span(start, stream.pos - 1),
        ));
    }

    if stream.peek_seq("EMPTY") {
        ctx.element_types.push(ElementTypeDecl {
            name,
            content_spec: ContentSpec::Empty,
        });
        stream.advance(5);
    } else if stream.peek_seq("ANY") {
        ctx.element_types.push(ElementTypeDecl {
            name,
            content_spec: ContentSpec::Any,
        });
        stream.advance(3);
    } else if stream.peek_seq("(") {
        // TODO: Parse 'Mixed' & 'children' content specs
        stream.expect_byte(b'(')?;
        stream.consume_whitespace();

        if stream.peek_seq("#PCDATA") {
            parse_mixed_content(stream, ctx, name)?;
        } else {
            let content = parse_element_content(stream, ctx)?;
            ctx.element_types.push(ElementTypeDecl {
                name,
                content_spec: ContentSpec::ElementContent(content),
            });
        }
    } else {
        return Err(ParseError::InvalidElementTypeDecl(stream.span(start, stream.pos)));
    }

    stream.consume_whitespace();
    stream.expect_byte(b'>')?;

    Ok(())
}

// Mixed  ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
fn parse_mixed_content<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>, name: &'a str) -> ParseResult<()> {
    let mut names = vec![];

    let start = stream.pos;
    stream.advance(7);
    names.push(stream.slice(start, stream.pos));

    stream.consume_whitespace();

    loop {
        match stream.current_byte()? {
            b'|' => {
                stream.advance(1);
                stream.consume_whitespace();

                let name = parse_name(stream)?;

                // The same name MUST NOT appear more than once in a single mixed-content declaration.
                if names.contains(&name) {
                    return Err(ParseError::DuplicateMixedContent(
                        name.to_string(),
                        stream.span(stream.pos - name.len(), stream.pos),
                    ));
                }

                names.push(name);
                stream.consume_whitespace();
            }
            b')' => break,
            _ => unreachable!(
                "this should never happen but you should never say never so it should probably be handled but that is a future me problem"
            ),
        }
    }

    stream.expect_byte(b')')?;

    if names.len() > 1 {
        stream.expect_byte(b'*')?;
    }

    ctx.element_types.push(ElementTypeDecl {
        name,
        content_spec: ContentSpec::MixedContent(names),
    });

    Ok(())
}

fn parse_parameter_entity_ref<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    // TODO: Something with me
    stream.expect_byte(b'%')?;
    let _name = parse_name(stream)?;
    stream.expect_byte(b';')?;
    Ok(())
}

// EntityDecl  ::=  GEDecl | PEDecl
// GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
// PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
// EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
// PEDef       ::=  EntityValue | ExternalID
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(8);
    stream.expect_and_consume_whitespace()?;

    let name = parse_name(stream)?;
    stream.expect_and_consume_whitespace()?;
    let value = match stream.current_byte()? {
        b'\'' | b'"' => parse_entity_value(stream, name)?,
        b => return Err(ParseError::UnexpectedCharacter2("`'` or `\"`", b, stream.span_single())),
    };

    stream.expect_byte(b'>')?;

    // TODO: Does this need to be an error?
    // If the same entity is declared more than once, the first declaration encountered is binding
    if ctx.entities.iter().any(|entity| entity.name == name) {
        return Ok(());
    }

    ctx.entities.push(Entity { name, value });

    Ok(())
}

// EntityValue  ::=  '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
fn parse_entity_value<'a>(stream: &mut TokenStream<'a>, _name: &'a str) -> ParseResult<&'a str> {
    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    // TODO: Parse references and recursion detection
    //  - A parsed entity MUST NOT contain a recursive reference to it either directly or indirectly.
    loop {
        match stream.current_byte()? {
            d if d == delimiter => break,
            _ => stream.advance(1),
        }
    }

    stream.advance(1);
    let value = stream.slice(start, stream.pos);

    Ok(value)
}

fn parse_element<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>, kind: ElementType) -> ParseResult<()> {
    let start = stream.pos;
    let element_start = parse_element_start(stream, ctx)?;

    if let Some(doc_name) = ctx.doc.name
        && let ElementType::Root = kind
        && let Token::ElementStart { local, .. } = element_start
        && doc_name != local
    {
        return Err(ParseError::InvalidRootName(
            doc_name.to_string(),
            local.to_string(),
            stream.span(start + 1, stream.pos),
        ));
    }

    ctx.emit_token(stream, element_start)?;

    let mut is_open = false;
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b'>' => {
                ctx.emit_token(
                    stream,
                    Token::ElementEnd {
                        kind: ElementEndKind::Open,
                    },
                )?;
                stream.advance(1);
                is_open = true;
                break;
            }
            b'/' => {
                stream.advance(1);
                stream.expect_byte(b'>')?;
                ctx.emit_token(
                    stream,
                    Token::ElementEnd {
                        kind: ElementEndKind::Empty,
                    },
                )?;
                break;
            }
            _ => {
                // Attributes need a leading white space
                if !stream.is_white_space()? {
                    return Err(ParseError::UnexpectedCharacter2(
                        "a space",
                        stream.unchecked_current_byte(),
                        stream.span_single(),
                    ));
                }
                stream.consume_whitespace();

                let attribute = parse_attribute(stream, ctx)?;
                ctx.emit_token(stream, attribute)?;
            }
        }
    }

    if is_open {
        parse_content(stream, ctx)?;
    }

    Ok(())
}

// STag ::= '<' QName (S Attribute)* S? '>'
//          ^^^^^^^^^
fn parse_element_start<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<Token<'a>> {
    let start = stream.pos;
    stream.advance(1);

    if stream.is_white_space()? {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }

    let (prefix, local) = parse_qname(stream, ctx)?;

    if let Some(prefix) = prefix
        && prefix == XMLNS_PREFIX
    {
        return Err(ParseError::ReservedPrefix(stream.span(start, stream.pos)));
    }

    Ok(Token::ElementStart { prefix, local })
}

// Attribute  ::=  NSAttName Eq AttValue | QName Eq AttValue
// AttValue   ::=  '"' ([^<&"] | Reference)* '"'
//              |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<Token<'a>> {
    let (prefix, local) = parse_qname(stream, ctx)?;
    stream.consume_whitespace();
    stream.expect_byte(b'=')?;
    stream.consume_whitespace();
    let value = parse_attribute_value(stream)?;
    Ok(Token::Attribute { prefix, local, value })
}

fn parse_attribute_value<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    let delimiter = stream.consume_quote()?;
    let start = stream.pos;

    loop {
        match stream.current_byte() {
            Ok(d) if d == delimiter => break,
            Ok(c) if c == b'<' || c == b'&' => {
                // TODO: Handle Character/Entity references
                return Err(ParseError::UnexpectedCharacter(delimiter, c, stream.span_single()));
            }
            Ok(_) => stream.advance(1),
            Err(err) => return Err(err),
        }
    }

    stream.advance(1);
    Ok(stream.slice(start, stream.pos - 1))
}

// ETag  ::=  '</' QName S? '>'
fn parse_element_end<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(2);

    if stream.is_white_space()? {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }

    let (prefix, local) = parse_qname(stream, ctx)?;
    stream.expect_byte(b'>')?;

    ctx.emit_token(
        stream,
        Token::ElementEnd {
            kind: ElementEndKind::Close { prefix, local },
        },
    )?;

    Ok(())
}

// QName           ::=  PrefixedName | UnprefixedName
// PrefixedName    ::=  Prefix ':' LocalPart
// UnprefixedName  ::=  LocalPart
// Prefix          ::=  NCName
// LocalPart       ::=  NCName
// NCName          ::=  Name - (Char* ':' Char*) /* An XML Name, minus the ":" */
fn parse_qname<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<(Option<&'a str>, &'a str)> {
    let start = stream.pos;

    let local = parse_nc_name(stream)?;

    if stream.current_byte()? != b':' {
        if local.is_empty() {
            return Err(ParseError::InvalidXmlName(stream.span_single()));
        }
        return Ok((None, local));
    }

    let prefix = local;
    if prefix.is_empty() {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }

    // 'xml' prefix will be mapped to 'http://www.w3.org/XML/1998/namespace'
    if prefix != XML_PREFIX && !stream.has_prefix(ctx, prefix) {
        return Err(ParseError::UnknownPrefix(
            prefix.to_owned(),
            stream.span(start, stream.pos),
        ));
    }

    stream.expect_byte(b':')?;

    let local = parse_nc_name(stream)?;
    if local.is_empty() {
        return Err(ParseError::InvalidXmlName(stream.span_single()));
    }

    Ok((Some(prefix), local))
}

// content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    while !stream.is_at_end() {
        match stream.unchecked_current_byte() {
            b'<' if stream.starts_with("<?") => parse_processing_instruction(stream, ctx)?,
            b'<' if stream.starts_with("<![CDATA[") => parse_cdata(stream, ctx)?,
            b'<' if stream.starts_with("<!-- ") => parse_comment(stream, ctx)?,
            b'<' if stream.starts_with("</") => {
                parse_element_end(stream, ctx)?;
                break;
            }
            b'<' => parse_element(stream, ctx, ElementType::Child)?,
            _ => {
                if stream.is_white_space()? {
                    stream.advance(1);
                } else {
                    parse_text(stream, ctx)?;
                }
            }
        }
    }

    Ok(())
}

// CDSect   ::=  CDStart CData CDEnd
// CDStart  ::=  '<![CDATA['
// CData    ::=  (Char* - (Char* ']]>' Char*))
// CDEnd    ::=  ']]>'
fn parse_cdata<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance(9);
    let start = stream.pos;

    loop {
        if stream.is_at_end() {
            return Err(ParseError::UnexpectedEndOfStream);
        }

        if stream.unchecked_current_byte() == b']' && stream.peek_seq("]]>") {
            break;
        }

        stream.advance(1);
    }

    let end = stream.pos;
    let data = &stream.source[start..end]; // TODO: slice

    validate::is_xml_chars(data, start, stream)?;

    stream.advance(3);
    ctx.emit_token(stream, Token::CData { data })?;

    Ok(())
}

fn parse_text<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    let start = stream.pos;

    loop {
        match stream.current_byte() {
            Ok(b'<') => break,
            Ok(_) => stream.advance(1),
            Err(err) => return Err(err),
        }
    }

    let text = stream.slice(start, stream.pos);
    if text.contains("]]>") {
        let pos = text.find("]]>").expect("text to contain ]]>");
        let start = start + pos;
        return Err(ParseError::InvalidCharData(stream.span(start, start + 3)));
    }
    ctx.emit_token(stream, Token::Text { text })?;

    Ok(())
}

fn parse_comment<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    let start = stream.pos;

    stream.advance(5);
    loop {
        if stream.is_at_end() {
            return Err(ParseError::UnexpectedEndOfStream);
        }

        if let Ok(b'\n') = stream.current_byte() {
            let span = stream.span(start, stream.pos);
            return Err(ParseError::InvalidComment(String::from("unclosed comment"), span));
        }

        // For compatibility, the string "--" (double-hyphen) must not occur within comments.
        if stream.unchecked_current_byte() == b'-' && stream.peek_seq("--") {
            if stream.peek_seq("-->") {
                break;
            }

            let span = stream.span(start, stream.pos + 2);
            return Err(ParseError::InvalidComment(
                String::from("double-hyphens (--) are not allowed inside comments"),
                span,
            ));
        }

        stream.advance(1);
    }

    stream.advance(3);
    let comment = stream.slice(start, stream.pos);
    validate::is_xml_chars(comment, start, stream)?;
    ctx.emit_token(stream, Token::Comment { comment })?;

    Ok(())
}

// children     ::=  (choice | seq) ('?' | '*' | '+')?
// cp           ::=  (Name | choice | seq) ('?' | '*' | '+')?
// choice       ::=  '(' S? cp ( S? '|' S? cp )+ S? ')'         TODO: [VC: Proper Group/PE Nesting]
// seq          ::=  '(' S? cp ( S? ',' S? cp )* S? ')'         TODO: [VC: Proper Group/PE Nesting]
fn parse_element_content<'a>(
    stream: &mut TokenStream<'a>,
    ctx: &mut Context<'a>,
) -> ParseResult<Vec<ElementContent<'a>>> {
    let mut content = Vec::new();
    // Leading '(' and any whitespace was already consumed
    match stream.current_byte()? {
        b'%' => parse_parameter_entity_ref(stream, ctx)?,
        b'(' => {
            // Started with a nested content particle
            stream.advance(1);
            content.extend(parse_element_content(stream, ctx)?);
        }
        _ => {
            let name = parse_name(stream)?;
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

            content.push(ElementContent::Name { name, repetition });
        }
    }

    let mut content_type: Option<ElementContent> = None;
    loop {
        stream.consume_whitespace();

        match stream.current_byte()? {
            b'%' => parse_parameter_entity_ref(stream, ctx)?,
            b')' => break,
            b',' => {
                match content_type {
                    None => content_type = Some(ElementContent::Seq(Vec::new())),
                    Some(ElementContent::Choice(_)) => panic!("TODO: Invalid element content sep"),
                    Some(ElementContent::Name { .. }) => panic!("TODO: Invalid element content sep"),
                    Some(ElementContent::Seq(_)) => {}
                }
                stream.advance(1);
            }

            b'|' => {
                match content_type {
                    None => content_type = Some(ElementContent::Choice(Vec::new())),
                    Some(ElementContent::Seq(_)) => panic!("TODO: Invalid element content sep"),
                    Some(ElementContent::Name { .. }) => panic!("TODO: Invalid element content sep"),
                    Some(ElementContent::Choice(_)) => {}
                }
                stream.advance(1);
            }
            b'(' => {
                stream.advance(1);
                content.extend(parse_element_content(stream, ctx)?);
            }
            _ => {
                let name = parse_name(stream)?;
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

                let _particle = ElementContent::Name { name, repetition };
            }
        }
    }

    stream.expect_byte(b')')?;
    let _repetition = match stream.current_byte()? {
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

    Ok(content)
}

#[cfg(test)]
mod test {
    use super::*;

    fn context() -> Context<'static> {
        Context {
            doc: Document {
                name: None,
                root_node_id: None,
                nodes: Vec::new(),
                namespaces: Vec::new(),
            },
            temp_elements: Vec::new(),
            entities: Vec::new(),
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
                namespaces: Vec::new(),
            },
            temp_elements: vec![TempElementData {
                prefix: None,
                local: "local",
                namespaces: vec![],
                children: vec![],
                attributes: vec![],
                id: 0,
                parent_id: None,
            }],
            entities: Vec::new(),
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

        parse_processing_instruction(&mut stream, &mut ctx).unwrap();

        assert_eq!(
            NodeKind::ProcessingInstruction {
                target: "hello",
                data: Some("world"),
            },
            ctx.doc.nodes[0].data,
        );
    }

    #[test]
    fn test_parse_pi_empty() {
        let mut stream = stream_from(r#"<?hello?>"#);
        let mut ctx = context();
        parse_processing_instruction(&mut stream, &mut ctx).unwrap();
        assert_eq!(
            NodeKind::ProcessingInstruction {
                target: "hello",
                data: None,
            },
            ctx.doc.nodes[0].data,
        );
    }

    #[test]
    fn test_parse_pi_reserved_target() {
        let mut stream = stream_from(r#"<?xml world?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::UnexpectedDeclaration(_))));
    }

    #[test]
    fn test_parse_pi_invalid_target_leading_whitespace() {
        let mut stream = stream_from(r#"<? target world?>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::InvalidXmlName(_))));
    }

    #[test]
    fn test_parse_pi_unexpected_eos() {
        let mut stream = stream_from(r#"<?target"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::UnexpectedEndOfStream)));
    }

    #[test]
    fn test_parse_pi_invalid_name() {
        let mut stream = stream_from("<?L\u{FFFE}L hehe?>");
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::InvalidXmlChar(_, _))));
    }

    #[test]
    fn test_parse_pi_invalid_close_1() {
        let mut stream = stream_from(r#"<?target data?"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::UnexpectedEndOfStream)));
    }

    #[test]
    fn test_parse_pi_invalid_close_2() {
        let mut stream = stream_from(r#"<?target data?<a/>"#);
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::UnexpectedCharacter(_, _, _))));
    }

    #[test]
    fn test_parse_pi_invalid_data() {
        let mut stream = stream_from("<?target dat\u{FFFF}a?>");
        let mut ctx = context();
        let res = parse_processing_instruction(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::InvalidXmlChar(_, _))));
    }

    // ========== Namespace/QName ==========
    #[test]
    fn test_parse_stag_with_prefix() {
        let res = Parser::parse(r#"<root xmlns:hello="bob"><hello:world/></root>"#);
        assert!(res.is_ok(),);
    }

    #[test]
    fn test_parse_stag_reserved_prefix() {
        let mut stream = stream_from(r#"<xmlns:world/>"#);
        let mut ctx = context();
        let res = parse_element_start(&mut stream, &mut ctx);
        assert!(matches!(res, Err(ParseError::ReservedPrefix(_))));
    }

    #[test]
    fn test_parse_ns_decl() {
        let mut stream = stream_from(r#"xmlns:foo="https://www.google.com""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx).unwrap();
        assert_eq!(
            Token::Attribute {
                prefix: Some("xmlns"),
                local: "foo",
                value: "https://www.google.com"
            },
            res
        );
    }

    #[test]
    fn test_parse_ns_decl_default() {
        let mut stream = stream_from(r#"xmlns="https://www.google.com""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx).unwrap();
        assert_eq!(
            Token::Attribute {
                prefix: None,
                local: "xmlns",
                value: "https://www.google.com"
            },
            res
        );
    }

    #[test]
    fn test_parse_invalid_xml_prefix_uri() {
        let mut stream = stream_with_element(r#"xmlns:xml="https://www.google.com""#);
        let mut ctx = context_temp_elements();
        let token = parse_attribute(&mut stream, &mut ctx).unwrap();
        let res = ctx.emit_token(&mut stream, token);
        assert!(matches!(res, Err(ParseError::InvalidXmlPrefixUri(_))));
    }

    #[test]
    fn test_parse_unexpected_xml_uri() {
        let mut stream = stream_with_element(r#"xmlns:a="http://www.w3.org/XML/1998/namespace""#);
        let mut ctx = context_temp_elements();
        let token = parse_attribute(&mut stream, &mut ctx).unwrap();
        let res = ctx.emit_token(&mut stream, token);
        assert!(matches!(res, Err(ParseError::UnexpectedXmlUri(_))));
    }

    #[test]
    fn test_parse_unexpected_xmlns_uri() {
        let mut stream = stream_with_element(r#"xmlns:a="http://www.w3.org/2000/xmlns/""#);
        let mut ctx = context_temp_elements();
        let token = parse_attribute(&mut stream, &mut ctx).unwrap();
        let res = ctx.emit_token(&mut stream, token);
        assert!(matches!(res, Err(ParseError::UnexpectedXmlnsUri(_))));
    }

    #[test]
    fn test_parse_unexpected_xmlns_uri_default() {
        let mut stream = stream_with_element(r#"xmlns="http://www.w3.org/2000/xmlns/""#);
        let mut ctx = context_temp_elements();
        let token = parse_attribute(&mut stream, &mut ctx).unwrap();
        let res = ctx.emit_token(&mut stream, token);
        assert!(matches!(res, Err(ParseError::UnexpectedXmlnsUri(_))));
    }

    // ========== Element ==========
    #[test]
    fn test_parse_stag() {
        let mut stream = stream_from(r#"<hello/>"#);
        let mut ctx = context();
        let res = parse_element_start(&mut stream, &mut ctx).unwrap();
        assert_eq!(
            Token::ElementStart {
                prefix: None,
                local: "hello",
            },
            res,
        );
    }

    #[test]
    fn test_parse_element() {
        let res = Parser::parse(r#"<root xmlns:ns="ns"><ns:name>hello</ns:name></root>"#).unwrap();

        assert_eq!(
            Node {
                id: 0,
                parent_id: None,
                data: NodeKind::Element {
                    name: "root",
                    namespaces: vec![Namespace {
                        name: Some("ns"),
                        uri: "ns",
                        start: 0,
                        end: Some(3)
                    }],
                    attributes: vec![],
                    children: vec![Node {
                        id: 1,
                        parent_id: Some(0),
                        data: NodeKind::Element {
                            name: "name",
                            namespaces: vec![Namespace {
                                name: Some("ns",),
                                uri: "ns",
                                start: 0,
                                end: None,
                            },],
                            attributes: vec![],
                            children: vec![Node {
                                id: 2,
                                parent_id: Some(1),
                                data: NodeKind::Text("hello")
                            }]
                        }
                    }]
                }
            },
            res.nodes[0]
        );
    }

    #[test]
    fn test_parse_empty_element() {
        let res = Parser::parse(r#"<name/>"#).unwrap();

        assert_eq!(
            NodeKind::Element {
                name: "name",
                namespaces: vec![],
                attributes: vec![],
                children: vec![]
            },
            res.nodes[0].data
        );
    }

    #[test]
    fn test_parse_element_prefix_mismatch() {
        let res = Parser::parse(r#"<root xmlns:a="a" xmlns:b="b"><a:name></b:name></root>"#);
        assert!(matches!(res, Err(ParseError::TagNameMismatch(_, _, _))));
    }

    #[test]
    fn test_parse_element_name_mismatch() {
        let res = Parser::parse(r#"<name></notaname>"#);
        assert!(matches!(res, Err(ParseError::TagNameMismatch(_, _, _))));
    }

    #[test]
    fn test_parse_element_invalid_tag_name() {
        let res = Parser::parse(r#"< name/>"#);
        assert!(matches!(res, Err(ParseError::InvalidXmlName(_))));
    }

    #[test]
    fn test_parse_element_namespace_declaration() {
        let mut stream = stream_from(r#"<tag xmlns:foo="http://www.google.com"/>"#);
        let mut ctx = context();
        parse_element(&mut stream, &mut ctx, ElementType::Child).unwrap();

        assert_eq!(
            NodeKind::Element {
                name: "tag",
                namespaces: vec![Namespace {
                    name: Some("foo"),
                    uri: "http://www.google.com",
                    start: 0,
                    end: Some(1),
                }],
                attributes: vec![],
                children: vec![]
            },
            ctx.doc.nodes[0].data
        );
    }

    #[test]
    fn test_parse_element_default_namespace_declaration() {
        let mut stream = stream_from(r#"<tag xmlns="http://www.google.com"/>"#);
        let mut ctx = context();
        parse_element(&mut stream, &mut ctx, ElementType::Child).unwrap();

        assert_eq!(
            NodeKind::Element {
                name: "tag",
                namespaces: vec![Namespace {
                    name: None,
                    uri: "http://www.google.com",
                    start: 0,
                    end: Some(1),
                }],
                attributes: vec![],
                children: vec![]
            },
            ctx.doc.nodes[0].data
        );
    }

    #[test]
    fn test_parse_element_with_attributes() {
        let mut stream = stream_from(r#"<tag some="value" another="one"></tag>"#);
        let mut ctx = context();
        parse_element(&mut stream, &mut ctx, ElementType::Child).unwrap();

        assert_eq!(
            NodeKind::Element {
                name: "tag",
                namespaces: vec![],
                attributes: vec![
                    Attribute {
                        name: "some",
                        value: "value"
                    },
                    Attribute {
                        name: "another",
                        value: "one"
                    }
                ],
                children: vec![]
            },
            ctx.doc.nodes[0].data
        );
    }

    #[test]
    fn test_parse_element_empty_with_attributes() {
        let mut stream = stream_from(r#"<name some="value" another="one"/>"#);
        let mut ctx = context();
        parse_element(&mut stream, &mut ctx, ElementType::Child).unwrap();

        assert_eq!(
            NodeKind::Element {
                name: "name",
                namespaces: vec![],
                attributes: vec![
                    Attribute {
                        name: "some",
                        value: "value"
                    },
                    Attribute {
                        name: "another",
                        value: "one"
                    }
                ],
                children: vec![]
            },
            ctx.doc.nodes[0].data
        );
    }

    // ========== Attributes ==========

    #[test]
    fn test_parse_attribute() {
        let mut stream = stream_from(r#"b="c""#);
        let mut ctx = context();
        let res = parse_attribute(&mut stream, &mut ctx).unwrap();
        assert_eq!(
            Token::Attribute {
                prefix: None,
                local: "b",
                value: "c"
            },
            res
        );
    }

    #[test]
    fn test_parse_element_duplicate_attribute() {
        let res = Parser::parse(r#"<name some="value" some="value"/>"#);
        assert!(matches!(res, Err(ParseError::DuplicateAttribute(_, _))));
    }
}
