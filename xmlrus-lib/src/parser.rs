use crate::error::ParseResult;
use crate::lexer::Lexer;
use crate::lexer::TokenKind;
use crate::lexer::TokenizedType;

struct TokenStream<'a> {
    lexer: Lexer<'a>,
}

impl<'a> TokenStream<'a> {
    fn current(&self) -> TokenKind<'a> {
        self.lexer.current_token().kind
    }

    fn previous(&self) -> Option<TokenKind<'a>> {
        Some(self.lexer.prev_token()?.kind)
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.lexer.current_token().kind == kind
    }

    fn advance(&mut self) {
        let _ = self.lexer.next_token();
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<()> {
        let current = self.current();
        if current != kind {
            panic!("Expected {kind:?}, got {current:?}")
        }

        self.advance();
        Ok(())
    }

    fn expect_whitespace(&mut self, reason: &str) -> ParseResult<()> {
        if let TokenKind::Whitespace(_) = self.current() {
            self.advance();
            Ok(())
        } else {
            panic!("Missing expected whitespace {reason}");
        }
    }

    fn expect_preceeding_whitespace(&self, before: &str) -> ParseResult<()> {
        if let Some(TokenKind::Whitespace(_)) = self.previous() {
            Ok(())
        } else {
            panic!("Missing expected whitespace before {before}")
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
            self.advance();
            Ok(name)
        } else {
            panic!("expected name")
        }
    }

    fn expect_and_get_comment(&mut self) -> ParseResult<&'a str> {
        if let TokenKind::Comment(comment) = self.current() {
            self.advance();
            Ok(comment)
        } else {
            panic!("expected comment")
        }
    }

    fn expect_and_get_nmtoken(&mut self) -> ParseResult<&'a str> {
        if let TokenKind::NmToken(nmtoken) = self.current() {
            self.advance();
            Ok(nmtoken)
        } else {
            panic!("expected nmtoken")
        }
    }

    fn consume_whitespace(&mut self) -> bool {
        if let TokenKind::Whitespace(_) = self.current() {
            self.advance();
            return true;
        }

        false
    }
}

pub fn parse<'a>(source: &'a str) {
    let lexer = Lexer::new(source);
    let mut stream = TokenStream { lexer };
    let _ = parse_document(&mut stream);
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
    unimplemented!("parse_entity_value")
}

/// [10]  AttValue  ::=  '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute_value<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_attribute_value")
}

/// [11] SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn parse_system_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_system_literal")
}

/// [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
/// [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn parse_public_id_literal<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_public_id_literal")
}

/// [14]  CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn parse_chardata<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_chardata")
}

/// [15]  Comment  ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
fn parse_comment<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO: Add node
    let _comment = stream.expect_and_get_comment()?;
    Ok(())
}

/// [16] PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
/// [17] PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::PIStart)?;
    let _name = stream.expect_and_get_name()?;

    stream.consume_whitespace();

    if let TokenKind::CharData(_cdata) = stream.current() {
        stream.expect_preceeding_whitespace("PI CharData")?;
        stream.advance();
    }

    // TODO: Add node
    stream.expect(TokenKind::PIEnd)?;
    Ok(())
}

/// [18] CDSect   ::=  CDStart CData CDEnd
/// [19] CDStart  ::=  '<![CDATA['
/// [20] CData    ::=  (Char* - (Char* ']]>' Char*))
/// [21] CDEnd    ::=  ']]>'
fn parse_cdata<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_cdata")
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
    stream.expect(TokenKind::XmlDeclStart)?;
    stream.expect_whitespace("before document version")?;
    stream.expect(TokenKind::Version)?;
    stream.expect(TokenKind::Equal)?;

    let version = stream.expect_and_get_literal()?;
    if !version.starts_with("1.0") {
        panic!("version must be 1.x")
    }

    let _encoding = {
        let ws = stream.consume_whitespace();
        if let TokenKind::Encoding = stream.current() {
            if !ws {
                panic!("missing required ws before encoding");
            }
            stream.advance();
            stream.expect(TokenKind::Equal)?;

            // TODO: encoding validation
            let encoding = stream.expect_and_get_literal();
            Some(encoding)
        } else {
            None
        }
    };

    let _standalone = {
        let ws = stream.consume_whitespace();
        if let TokenKind::Standalone = stream.current() {
            if !ws {
                panic!("missing required ws before standalone");
            }
            stream.advance();
            stream.expect(TokenKind::Equal)?;

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
    stream.expect(TokenKind::XmlDeclEnd)?;

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
    stream.expect_whitespace("before DTD name")?;

    // TODO: add document name
    let _name = stream.expect_and_get_name()?;
    if let Some((_system_id, _public_id)) = parse_external_id(stream, false)? {
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
    unimplemented!("parse_element")
}

/// [40] STag          ::= '<' QName (S Attribute)* S? '>'
/// [44] EmptyElemTag  ::= '<' QName (S Attribute)* S? '/>'
fn parse_element_start<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::OpenTagStart)?;
    let _name = stream.expect_and_get_name()?;
    unimplemented!("parse_element_start")
}

/// [42]  ETag  ::=  '</' Name S? '>'
fn parse_etag<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_etag")
}

/// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_content")
}

/// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
/// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("before ElementDecl Name")?;

    let _name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after ElementDecl Name")?;

    match stream.current() {
        TokenKind::Empty => (),
        TokenKind::Any => (),
        TokenKind::LeftParen => {
            stream.advance();
            stream.consume_whitespace();
            if let TokenKind::PCData = stream.current() {
                parse_mixed(stream)?;
            } else {
                parse_element_content_children(stream)?;
            }
        }
        kind => panic!("expected [Empty | Any | '('], got {kind:?}"),
    }

    Ok(())
}

/// [47] children  ::=   (choice | seq) ('?' | '*' | '+')?
/// [48] cp        ::=   (Name | choice | seq) ('?' | '*' | '+')?
/// [49] choice    ::=   '(' S? cp ( S? '|' S? cp )+ S? ')'	[VC: Proper Group/PE Nesting]
/// [50] seq       ::=   '(' S? cp ( S? ',' S? cp )* S? ')'	[VC: Proper Group/PE Nesting]
fn parse_element_content_children<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // Leading '(' and any whitespace was already consumed
    unimplemented!("parse_element_content_children")
}

/// [51] Mixed  ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
fn parse_mixed<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_mixed")
}

/// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("after AttlistDecl")?;

    let _name = stream.expect_and_get_name()?;
    loop {
        match stream.current() {
            TokenKind::MarkupDeclEnd | TokenKind::Eof => break,
            TokenKind::Whitespace(_) => stream.advance(),
            _ => {
                // TODO: Do something with AttDef
                parse_att_def(stream)?;
            }
        }
    }

    // TODO: Add AttributeDecl
    stream.expect(TokenKind::MarkupDeclEnd)?;

    Ok(())
}

/// [53] AttDef  ::=  S Name S AttType S DefaultDecl
fn parse_att_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.consume_whitespace();

    if let TokenKind::Name(_name) = stream.current() {
        stream.expect_preceeding_whitespace("AttDef name")?;
        stream.advance();
        stream.expect_whitespace("after AttDef name")?;
        parse_att_type(stream)?;
        stream.expect_whitespace("after AttType")?;
        parse_default_decl(stream)?;
    }

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
    match stream.current() {
        TokenKind::CData => (),
        TokenKind::TokenizedType(tt) => {
            // TODO: Something with me
            match tt {
                TokenizedType::Id => (),
                TokenizedType::IdRef => (),
                TokenizedType::IdRefs => (),
                TokenizedType::Entity => (),
                TokenizedType::Entities => (),
                TokenizedType::NmToken => (),
                TokenizedType::NmTokens => (),
            }
            stream.advance();
        }
        _ => parse_enumerated_type(stream)?,
    }

    Ok(())
}

/// [57]  EnumeratedType  ::=  NotationType | Enumeration
fn parse_enumerated_type<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    match stream.current() {
        TokenKind::NotationType => parse_notation_type(stream)?,
        TokenKind::Enumeration => parse_enumeration(stream)?,
        kind => panic!("expected [NotationType | Enumeration], got {kind:?}"),
    }

    Ok(())
}

/// [58]  NotationType  ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
fn parse_notation_type<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("after NOTATION")?;
    stream.expect(TokenKind::LeftParen)?;
    stream.consume_whitespace();

    let mut names = vec![stream.expect_and_get_name()?];

    loop {
        match stream.current() {
            TokenKind::RightParen => break,
            TokenKind::Whitespace(_) => stream.advance(),
            TokenKind::Pipe => {
                stream.advance();
                stream.consume_whitespace();
                names.push(stream.expect_and_get_name()?);
            }
            kind => panic!("expected [Name | '|' | ')'], got {kind:?}"),
        }
    }

    // TODO: Collect names and generate AttType::Enumerated(EnumeratedType::NotationType(names))
    stream.expect(TokenKind::RightParen)?;

    Ok(())
}

/// [59]  Enumeration  ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
fn parse_enumeration<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect(TokenKind::LeftParen)?;
    stream.consume_whitespace();

    let mut nmtokens = vec![stream.expect_and_get_nmtoken()?];

    loop {
        match stream.current() {
            TokenKind::RightParen => break,
            TokenKind::Whitespace(_) => stream.advance(),
            TokenKind::Pipe => {
                stream.advance();
                stream.consume_whitespace();
                nmtokens.push(stream.expect_and_get_nmtoken()?);
            }
            kind => panic!("expected [NmToken | '|' | ')'], got {kind:?}"),
        }
    }

    // TODO: Collect names and generate AttType::Enumerated(EnumeratedType::Enumeration(nmtokens))
    stream.expect(TokenKind::RightParen)?;

    Ok(())
}

/// [60]  DefaultDecl  ::=  '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
fn parse_default_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO: Return type
    match stream.current() {
        TokenKind::Required => {
            stream.advance();
            // Ok(DefaultDecl::Required)
            Ok(())
        }
        TokenKind::Implied => {
            stream.advance();
            // Ok(DefaultDecl::Implied)
            Ok(())
        }
        TokenKind::Fixed => {
            stream.advance();
            let current = stream.current();
            if let TokenKind::Literal(_value) = current {
                // Ok(DefaultDecl::Fixed { fixed: true, value })
                Ok(())
            } else {
                panic!("expected AttValue, got {current:?}")
            }
        }
        TokenKind::Literal(_value) => {
            // Ok(DefaultDecl::Fixed { fixed: false, value})
            Ok(())
        }
        kind => panic!("expected DefaultDecl, got {kind:?}"),
    }
}

/// [69]  PEReference  ::= '%' Name ';'
fn parse_pe_reference<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::Percent)?;
    let _name = stream.expect_and_get_name()?;
    stream.expect(TokenKind::SemiColon)?;

    // TODO: add reference
    Ok(())
}

/// [70] EntityDecl  ::=  GEDecl | PEDecl
/// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::EntityDecl)?;
    stream.expect_whitespace("after EntityDecl")?;

    if let TokenKind::Percent = stream.current() {
        parse_pe_def(stream)?;
    } else {
        parse_entity_def(stream)?;
    }

    stream.consume_whitespace();
    stream.expect(TokenKind::MarkupDeclEnd)?;

    Ok(())
}

/// [73] EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn parse_entity_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    let _name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after EntityDecl name")?;

    if let TokenKind::Literal(_entity_value) = stream.current() {
        // TODO: Add EntityType::InternalGeneral { entity_value }
        stream.advance();
        return Ok(());
    }

    // `parse_external_id()` will already bubble an error for a missing/malformed ExternalId
    // so it should be safe to use `expect()` here.
    let (_system_id, _public_id) = parse_external_id(stream, true)?.expect("missing required ExternalId in EntityDef");
    stream.consume_whitespace();

    if let TokenKind::NData = stream.current() {
        // TODO: Add EntityType::ExernalGeneralUnparsed { system_id, public_id, ndata}
        let _ndata = parse_ndata_decl(stream)?;
    } else {
        // TODO: Add EntityType::ExernalGeneralParsed { system_id, public_id }
    }

    Ok(())
}

/// [74]  PEDef  ::=  EntityValue | ExternalID
fn parse_pe_def<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_pe_def")
}

/// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(
    stream: &mut TokenStream<'a>,
    required: bool,
) -> ParseResult<Option<(&'a str, Option<&'a str>)>> {
    stream.consume_whitespace();

    if stream.is(TokenKind::System) {
        stream.advance();
        stream.expect_whitespace("ExternalID SystemLiteral")?;
        let system_id = stream.expect_and_get_literal()?;
        return Ok(Some((system_id, None)));
    }

    if stream.is(TokenKind::Public) {
        stream.advance();
        stream.expect_whitespace("ExternalId PubidLiteral")?;
        let public_id = stream.expect_and_get_literal()?;
        stream.expect_whitespace("ExternalID SystemLiteral")?;
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
fn parse_ndata_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<&'a str> {
    stream.expect_preceeding_whitespace("NDATA")?;
    stream.expect(TokenKind::NData)?;
    stream.expect_whitespace("after NDATA")?;
    Ok(stream.expect_and_get_name()?)
}

/// [82] NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
/// [83] PublicID      ::=  'PUBLIC' S PubidLiteral
fn parse_notation_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    unimplemented!("parse_notation_decl")
}
