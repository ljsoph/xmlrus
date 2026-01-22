use std::fmt::Display;

pub type ParseResult<T> = std::result::Result<T, Error>;

pub(crate) fn io(err: std::io::Error) -> Error {
    Error::new(ErrorKind::Io(err), TextPos { row: 0, col: 0 })
}

pub(crate) fn syntax(err: SyntaxError, stream: &crate::TokenStream) -> Error {
    Error::new(ErrorKind::Syntax(err), TextPos::new(stream))
}

pub(crate) fn validation(err: ValidationError, stream: &crate::TokenStream) -> Error {
    Error::new(ErrorKind::Invalid(err), TextPos::new(stream))
}

pub(crate) fn eof() -> Error {
    Error::new(ErrorKind::Eof, TextPos { row: 0, col: 0 })
}

#[derive(Debug)]
struct TextPos {
    row: usize,
    col: usize,
}

impl TextPos {
    fn new(stream: &crate::TokenStream) -> Self {
        let mut row = 1;
        let mut col = 1;

        for (pos, c) in stream.source.char_indices() {
            if pos == stream.pos {
                break;
            }

            if c == '\n' {
                row += 1;
                col = 1;
            } else {
                col += c.len_utf8()
            }
        }

        Self { row, col }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pos: TextPos,
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            ErrorKind::Io(io_error) => io_error.source(),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.kind)
    }
}

impl Error {
    fn new(kind: ErrorKind, pos: TextPos) -> Self {
        Self { kind, pos }
    }

    pub fn diagnostic(&self) -> String {
        format!("{}:{}: error: {}", self.pos.row, self.pos.col, self.kind)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    /// Error occurred when loading source XML
    Io(std::io::Error),
    /// Document is not Well-Formed
    Syntax(SyntaxError),
    /// Document is not Valid
    Invalid(ValidationError),
    /// Unexpected EOF while parsing
    Eof,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Io(io_error) => io_error.fmt(f),
            ErrorKind::Syntax(syntax_error) => syntax_error.fmt(f),
            ErrorKind::Invalid(validation_error) => validation_error.fmt(f),
            ErrorKind::Eof => f.write_str("unexpected EOF"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxError {
    /// An attribute name MUST NOT appear more than once in the same start-tag or empty-element tag.
    DuplicateAttribute {
        name: String,
    },

    /// The Name in an element's end-tag MUST match the element type in the start-tag.
    ElementNameMismatch {
        expected: String,
        actual: String,
    },

    /// Attribute values MUST NOT contain direct or indirect entity references to external entities.
    ExternalEntityRefInAttribute {
        ref_: String,
    },

    /// An entity reference MUST NOT contain the name of an unparsed entity.
    /// Unparsed entities may be referred to only in attribute values declared to be of type ENTITY or ENTITIES.
    IllegalUnparsedEntity {
        ref_: String,
    },

    /// Parameter-entity references MUST NOT appear outside the DTD.
    IllegalParameteEntityRef {
        ref_: String,
    },

    /// Declared Attribute Type was not a string type, a set of tokenized types, or an enumerated type.
    InvalidAttributeType,

    /// Encountered an invalid value when parsing a multi-byte character
    InvalidChar {
        value: u32,
    },

    /// [CharData] contained the CDATA-section-close delimiter `]]>`
    InvalidCharData,

    /// Characters referred to using character references MUST match the production for Char.
    InvalidCharRef {
        reason: String,
    },

    /// Invalid Comment
    InvalidComment,

    /// Groupings must use the same pattern separator - Choice ['|'] or Sequence [',']
    InvalidElementContentSeparator {
        expected: char,
        actual: char,
    },

    /// XML Declaration contained an invalid [Encoding]
    InvalidEncodingName,

    /// Invalid NmToken
    InvalidNmToken,

    /// Mising ExternalId or PublicId
    InvalidNotationDecl {
        reason: &'static str,
    },

    //// Invalid Public Identifier Literal
    InvalidPublicIdLiteral {
        c: char,
    },

    /// Valid values [yes | no]
    InvalidStandlone {
        value: String,
    },

    /// Invalid XML Version
    InvalidVersion {
        version: String,
    },

    /// Invalid XML character
    InvalidXmlChar {
        c: char,
    },

    /// Invalid XXMLL Name
    InvalidXmlName,

    /// The `xml` prefix must be bound to the `http://www.w3.org/XML/1998/namespace` namespace name
    InvalidXmlPrefixUri,

    MalformedElementContent {
        reason: &'static str,
    },

    /// Well formed Element Type declarations must contain a valid name and [Content Spec]
    MalformedElementTypeDecl,

    MalformedEntityReference {
        reason: &'static str,
    },

    /// A Document must have at least one element
    MissingRoot,

    /// XML Declaration did not contain the [Version] attribute
    MissingVersion,

    /// Missing Required External Identifier
    MissingRequiredExternalId,

    /// Missing Required Whitespace
    MissingRequiredWhitespace {
        at: &'static str,
    },

    /// A parsed entity MUST NOT contain a recursive reference to itself, either directly or indirectly.
    RecursiveEntityReference {
        ref_: String,
    },

    /// `xmlns` is a reserved Namespace and must not be used
    ReservedPrefix,

    /// In a namespace declaration for a prefix (i.e., where the NSAttName is a PrefixedAttName), the attribute value MUST NOT be empty.
    ///
    /// ```xml
    /// <a xmlns:foo=""/>
    /// ```
    ///
    /// Invalid in XML Namespaces 1.0 but allowed in 1.1
    UnboundXmlnsPrefix,

    /// Missing required '?>'
    UnclosedDeclaration,

    /// The replacement text of any entity referred to directly or indirectly in an attribute value MUST NOT contain a <.
    UnescapedLTInAttrValue,

    /// If present, the Declaration must be at the start of the Document
    UnexpectedDeclaration,

    /// Whatcha doin there little guy?
    UnexpectedCharacter {
        expected: String,
        actual: char,
    },

    /// An element besides a comment or PI was encountered after the root element
    UnexpectedElement,

    /// Prefixes other than `xml` MUST NOT be bound to the `http://www.w3.org/XML/1998/namespace`
    /// namespace name
    UnexpectedXmlUri,

    /// The URI `http://www.w3.org/2000/xmlns/` is bound to the `xmlns` prefix and MUST NOT be declared
    UnexpectedXmlnsUri,
}

impl std::error::Error for SyntaxError {}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateAttribute { name } => writeln!(f, "duplicate attribute '{name}'"),
            Self::ElementNameMismatch { expected, actual } => {
                writeln!(f, "mismatched start and end tag: expected '{expected}' got '{actual}'")
            }
            Self::ExternalEntityRefInAttribute { ref_ } => writeln!(f, "attribute '{ref_}' refrecnes external entity"),
            Self::IllegalUnparsedEntity { ref_ } => writeln!(f, "unparsed entity '{ref_}' outside of attribute value"),
            Self::IllegalParameteEntityRef { ref_ } => writeln!(f, "illegal parameter entity reference {ref_}"),
            Self::InvalidAttributeType => writeln!(f, "invalid attribute def"),
            Self::InvalidChar { value } => writeln!(f, "invalid character 'U+{value:04x}'"),
            Self::InvalidCharData => writeln!(f, "CharData cannot contain ']]>'"),
            Self::InvalidCharRef { reason } => writeln!(f, "invalid character reference: {reason}"),
            Self::InvalidComment => writeln!(
                f,
                "invalid comment: double-hyphens (--) are not allowed inside comments"
            ),
            Self::InvalidElementContentSeparator { expected, actual } => {
                writeln!(f, "invalid content separator: expected '{expected}' got '{actual}'")
            }
            Self::InvalidEncodingName => writeln!(f, "invalid encoding"),
            Self::InvalidNmToken => writeln!(f, "invalid NmToken"),
            Self::InvalidNotationDecl { reason } => writeln!(f, "invalid Notation declaration: {reason}"),
            Self::InvalidPublicIdLiteral { c } => writeln!(f, "invalid PubidLiteral '{c}"),
            Self::InvalidStandlone { value } => writeln!(f, "invalid Standalone '{value}"),
            Self::InvalidVersion { version } => writeln!(f, "invalid Version '{version}"),
            Self::InvalidXmlChar { c } => writeln!(f, "invalid XML char {c:#?}"),
            Self::InvalidXmlName => writeln!(f, "invalid XML name"),
            Self::InvalidXmlPrefixUri => writeln!(f, "invalid XML prefix"),
            Self::MalformedElementContent { reason } => writeln!(f, "malformed element content: {reason}"),
            Self::MalformedElementTypeDecl => writeln!(f, "malformed elmement type declaration"),
            Self::MalformedEntityReference { reason } => writeln!(f, "malformed entity reference: {reason}"),
            Self::MissingRoot => writeln!(f, "document must contain at least one element"),
            Self::MissingVersion => writeln!(f, "missing required version attribute"),
            Self::MissingRequiredExternalId => writeln!(f, "missing required External Identifier"),
            Self::MissingRequiredWhitespace { at } => writeln!(f, "missing required whitespace {at}"),
            Self::RecursiveEntityReference { ref_ } => writeln!(f, "recursive entity reference detected in '{ref_}"),
            Self::ReservedPrefix => writeln!(f, "'xmlns' is a reserved prefix"),
            Self::UnboundXmlnsPrefix => writeln!(f, "XML namespace must not be empty"),
            Self::UnclosedDeclaration => writeln!(f, "unclosed declaration"),
            Self::UnescapedLTInAttrValue => writeln!(f, "unescaped '<' in attribute value"),
            Self::UnexpectedDeclaration => writeln!(f, "unexpected XML declaration"),
            Self::UnexpectedCharacter { expected, actual } => {
                writeln!(f, "unexpected character: expected '{expected}' got '{actual}'")
            }
            Self::UnexpectedElement => writeln!(f, "unexpected element"),
            Self::UnexpectedXmlUri => writeln!(f, "unexpected XML URI"),
            Self::UnexpectedXmlnsUri => writeln!(f, "unexpected XMLNS URI"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValidationError {
    /// An element type MUST NOT be declared more than once.
    DuplicateElementType { name: String },

    /// TODO: Is this even an error?
    DuplicateNamespace { name: String },

    /// The same name MUST NOT appear more than once in a single mixed-content declaration.
    DuplicateMixedContent { name: String },

    /// A given Name MUST NOT be declared in more than one notation declaration.
    DuplicateNotation { name: String },

    /// The Name in the document type declaration MUST match the element type of the root element.
    InvalidRootElementType { expected: String, actual: String },

    /// The standalone document declaration MUST have the value "no" if any external markup declarations contain declarations of:
    /// - attributes with default values, if elements to which these attributes apply appear in the document without specifications of values for these attributes, or
    /// - entities (other than amp, lt, gt, apos, quot), if references to those entities appear in the document, or
    /// - attributes with tokenized types, where the attribute appears in the document with a value such that normalization will produce a different value from that which would be produced in the absence of the declaration, or
    /// - element types with element content, if white space occurs directly within any instance of those types.
    StandaloneDocument,

    /// An Entity Reference was used that was not previously declared
    UnknownEntityReference { ref_: String },
    /// Unknown Prefix
    ///
    /// An Namespace prefix was used that was not previously declared or in scope
    UnknownPrefix { name: String },
}

impl std::error::Error for ValidationError {}

impl Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateElementType { name } => {
                writeln!(f, "duplicate element type declaration '{name}'")
            }
            Self::DuplicateNamespace { name } => writeln!(f, "duplicate namespace '{name}'"),
            Self::DuplicateMixedContent { name } => {
                writeln!(f, "duplicate mixed-content declaration '{name}'")
            }
            Self::DuplicateNotation { name } => writeln!(f, "duplicate notation declaration '{name}'"),
            Self::InvalidRootElementType { expected, actual } => {
                writeln!(f, "invalid root element name: expected '{expected}' got '{actual}'")
            }
            Self::StandaloneDocument => writeln!(f, "standlone document TODO"),
            Self::UnknownEntityReference { ref_ } => writeln!(f, "unknown entity reference '{ref_}"),
            Self::UnknownPrefix { name } => writeln!(f, "unknown prefix '{name}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NamespaceError {
    /// No tag may contain two attributes which have qualified names with the same local part
    /// and with prefixes which have been bound to namespace names that are identical.
    ///
    /// For example, each of the bad empty-element tags is illegal in the following:
    ///
    /// ```xml
    /// <!-- http://www.w3.org is bound to n1 and n2 -->
    /// <x xmlns:n1="http://www.w3.org"
    ///    xmlns:n2="http://www.w3.org" >
    ///   <bad a="1"     a="2" />
    ///   <bad n1:a="1"  n2:a="2" />
    /// </x>
    /// ```
    ///
    /// However, each of the following is legal, the second because the default namespace does not apply to attribute names:
    ///
    /// ```xml
    /// <!-- http://www.w3.org is bound to n1 and is the default -->
    /// <x xmlns:n1="http://www.w3.org"
    ///    xmlns="http://www.w3.org" >
    ///   <good a="1"     b="2" />
    ///   <good a="1"     n1:a="2" />
    /// </x>
    /// ```
    DuplicateNamespaceAttribute,
}

impl std::error::Error for NamespaceError {}

impl Display for NamespaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match self {}
        Ok(())
    }
}

// TODO: What to do with you?
// #[derive(Debug, PartialEq)]
// pub struct Span {
//     row: usize,
//     col_start: usize,
//     col_end: usize,
//     line: String,
// }
//
// impl std::fmt::Display for Span {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let row_offset = (self.row.checked_ilog10().unwrap_or(0) + 1) as usize;
//         let col_offset = self.col_start - 1;
//         let err_len = self.col_end - self.col_start;
//
//         writeln!(f, " {:>row_offset$} | ", "")?;
//         writeln!(f, " {:>row_offset$} | {}", self.row, self.line)?;
//         writeln!(f, " {:>row_offset$} | {:>col_offset$}{:^>err_len$}", "", "", "^")?;
//         write!(f, " {:>row_offset$} | ", "")
//     }
// }
//
// impl Span {
//     // TODO: Adjust diagnostic reporting to account for Unicode characters
//     fn new(source: &str, start: usize, end: usize) -> Self {
//         let mut row = 1;
//         let mut col = 1;
//         for (i, c) in source.chars().enumerate() {
//             if i == start {
//                 break;
//             }
//
//             if c == '\n' {
//                 col = 1;
//                 row += 1;
//                 continue;
//             }
//
//             col += 1;
//         }
//
//         let line = source.split('\n').nth(row - 1).unwrap_or_default().to_string();
//
//         Self {
//             row,
//             col_start: col,
//             col_end: col + (end - start),
//             line,
//         }
//     }
// }
