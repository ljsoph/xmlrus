use crate::error;
use crate::error::ParseResult;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::parse_name;

struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
    pos: usize,
}

impl<'a> TokenStream<'a> {
    fn current(&self) -> TokenKind<'a> {
        self.tokens[self.pos].kind
    }

    fn previous(&self) -> TokenKind<'a> {
        self.tokens[self.pos - 1].kind
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<()> {
        let current = self.current();
        if current != kind {
            panic!("Expected {kind:?}, got {current:?}")
        }

        self.advance();
        Ok(())
    }

    fn consume_whitespace(&mut self) -> bool {
        if let TokenKind::Whitespace(_) = self.current() {
            self.advance();
            return true;
        }

        false
    }

    fn expect_whitespace(&mut self) -> ParseResult<()> {
        if let TokenKind::Whitespace(_) = self.current() {
            self.advance();
            Ok(())
        } else {
            panic!("Expected whitespace");
        }
    }

    fn expect_and_get_literal(&mut self) -> ParseResult<&'a str> {
        if let TokenKind::Literal(literal) = self.current() {
            self.advance();
            Ok(literal)
        } else {
            panic!("expected literal")
        }
    }

    fn expect_and_get_name(&mut self) -> ParseResult<&'a str> {
        if let TokenKind::Name(name) = self.current() {
            Ok(name)
        } else {
            panic!("expected name")
        }
    }
}

pub fn parse<'a>(tokens: Vec<Token<'a>>) {
    let mut stream = TokenStream { tokens, pos: 0 };
    parse_document(&mut stream);
}

/// Parses the source input, emitting a stream of tokens to build up the
/// resulting `Document`
///
/// [1] Document  ::=  prolog element Misc*
fn parse_document<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    parse_prolog(stream)?;

    stream.consume_whitespace();

    if stream.is(TokenKind::Eof) {
        panic!("missing root element")
    }

    parse_element(stream)?;
    parse_misc(stream)?;

    if !stream.is(TokenKind::Eof) {
        panic!("unexpected element at end of document")
    }

    Ok(())
}

/// [9] EntityValue  ::=  '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
fn parse_entity_value<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [10]  AttValue  ::=  '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute_value<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [11] SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn parse_system_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
/// [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn parse_public_id_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [14]  CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn parse_chardata<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [15]  Comment  ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
fn parse_comment<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [16] PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
/// [17] PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [18] CDSect   ::=  CDStart CData CDEnd
/// [19] CDStart  ::=  '<![CDATA['
/// [20] CData    ::=  (Char* - (Char* ']]>' Char*))
/// [21] CDEnd    ::=  ']]>'
fn parse_cdata<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [22] prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if let TokenKind::XmlDeclStart = stream.current() {
        parse_xml_decl(stream)?;
    }

    parse_misc(stream)?;

    if stream.current() == TokenKind::DTDStart {
        parse_doc_type_decl(stream)?;
        parse_misc(stream)?
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
fn parse_xml_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::XmlDeclStart);
    stream.expect_whitespace()?;
    stream.expect(TokenKind::Version);
    stream.expect(TokenKind::Equal);

    let version = stream.expect_and_get_literal()?;
    if !version.starts_with("1.0") {
        panic!("version must be 1.x")
    }

    let encoding = {
        let ws = stream.consume_whitespace();
        if let TokenKind::Encoding = stream.current() {
            if !ws {
                panic!("missing required ws before encoding");
            }
            stream.advance();
            stream.expect(TokenKind::Equal);

            // TODO: encoding validation
            let encoding = stream.expect_and_get_literal();
            Some(encoding)
        } else {
            None
        }
    };

    let standalone = {
        let ws = stream.consume_whitespace();
        if let TokenKind::Standalone = stream.current() {
            if !ws {
                panic!("missing required ws before standalone");
            }
            stream.advance();
            stream.expect(TokenKind::Equal);

            // TODO: standalone validation
            let standalone = stream.expect_and_get_literal()?;
            if standalone != "yes" && standalone != "no" {
                panic!("invalid standalone")
            }

            Some(standalone)
        } else {
            None
        }
    };

    // TODO: add node
    stream.consume_whitespace();
    stream.expect(TokenKind::XmlDeclEnd);

    Ok(())
}

/// [27] Misc  ::=  Comment | PI | S
fn parse_misc<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    loop {
        // TODO: add nodes
        match stream.current() {
            TokenKind::Comment(_) => parse_comment(stream)?,
            TokenKind::PIStart => parse_processing_instruction(stream)?,
            TokenKind::Whitespace(_) => stream.advance(),
            _ => break,
        }
    }

    Ok(())
}

/// [28]  doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
/// [28a] DeclSep     ::=   PEReference | S
/// [28b] intSubset   ::=   (markupdecl | DeclSep)*
/// [29]  markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn parse_doc_type_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace()?;

    // TODO: add document name
    let name = stream.expect_and_get_name()?;
    if let Some((system_id, public_id)) = parse_external_id(stream, false)? {
        // TODO: add entity
    }

    stream.consume_whitespace();
    if stream.is(TokenKind::IntSubsetStart) {
        stream.advance();

        // parse 'intSubset'
        loop {
            match stream.current() {
                TokenKind::IntSubsetEnd => break,
                TokenKind::ElementDecl => parse_element_type_decl(stream)?,
                TokenKind::EntityDecl => parse_entity_decl(stream)?,
                TokenKind::AttlistDecl => parse_attlist_decl(stream)?,
                TokenKind::NotationDecl => parse_notation_decl(stream)?,
                TokenKind::PIStart => parse_processing_instruction(stream)?,
                TokenKind::Comment(_) => parse_comment(stream)?,
                TokenKind::Percent => parse_pe_reference(stream)?,
                TokenKind::Whitespace(_) => stream.advance(),
                TokenKind::Eof => panic!("unexpected EOF while parsing DTD"),
                kind => panic!("unexpected token in DTD: {kind:?}"),
            }
        }
    }

    stream.advance();
    stream.consume_whitespace();
    stream.expect(TokenKind::DTDEnd)?;

    Ok(())
}

/// [39] element  ::=  EmptyElemTag | STag content ETag
fn parse_element<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    parse_element_start(stream)?;

    Ok(())
}

/// [40] STag          ::= '<' QName (S Attribute)* S? '>'
/// [44] EmptyElemTag  ::= '<' QName (S Attribute)* S? '/>'
fn parse_element_start<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::OpenTagStart)?;
    let name = stream.expect_and_get_name()?;
    Ok(())
}

/// [42]  ETag  ::=  '</' Name S? '>'
fn parse_etag<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
/// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [53] AttDef         ::=  S Name S AttType S DefaultDecl
fn parse_att_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
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
fn parse_att_type<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [57]  EnumeratedType  ::=  NotationType | Enumeration
fn parse_enumerated_type<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [58]  NotationType  ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
fn parse_notation_type<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [59]  Enumeration  ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
fn parse_enumeration<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [69]  PEReference  ::= '%' Name ';'
fn parse_pe_reference<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::Percent);
    let name = stream.expect_and_get_name()?;
    stream.expect(TokenKind::SemiColon)?;

    // TODO: add reference
    Ok(())
}

/// [70] EntityDecl  ::=  GEDecl | PEDecl
/// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [73] EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn parse_entity_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [74]  PEDef  ::=  EntityValue | ExternalID
fn parse_pe_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(
    stream: &mut TokenStream<'a>,
    required: bool,
) -> ParseResult<Option<(&'a str, Option<&'a str>)>> {
    let has_ws = stream.consume_whitespace();

    if stream.is(TokenKind::System) {
        stream.expect_whitespace()?;
        let system_id = stream.expect_and_get_literal()?;
        return Ok(Some((system_id, None)));
    }

    if stream.is(TokenKind::Public) {
        stream.expect_whitespace()?;
        let public_id = stream.expect_and_get_literal()?;
        stream.expect_whitespace()?;
        let system_id = stream.expect_and_get_literal()?;

        return Ok(Some((system_id, Some(public_id))));
    }

    if required {
        panic!("missing required external id")
    } else {
        Ok(None)
    }
}

/// [76] NDataDecl  ::=  S 'NDATA' S Name
fn parse_ndata_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}

/// [82] NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
/// [83] PublicID      ::=  'PUBLIC' S PubidLiteral
fn parse_notation_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO
    Ok(())
}
