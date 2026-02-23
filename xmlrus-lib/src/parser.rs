use crate::context::Context;
use crate::document::Document;
use crate::document::Encoding;
use crate::document::EntityType;
use crate::document::XmlVersion;
use crate::error::ParseResult;
use crate::lexer::Lexer;
use crate::lexer::TokenKind;
use crate::lexer::TokenizedType;
use crate::validate;

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
    let mut stream = TokenStream {
        lexer: Lexer::new(source),
    };
    let mut ctx = Context {
        doc: Document::new(),
        validate: true,
        version: XmlVersion::V1_0,
        encoding: Encoding::Utf8,
        standalone: false,
    };
    let _ = parse_document(&mut stream, &mut ctx);

    dbg!(ctx);
}

/// Parses the source input, emitting a stream of tokens to build up the
/// resulting `Document`
///
/// [1] Document  ::=  prolog element Misc*
fn parse_document<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    parse_prolog(stream, ctx)?;

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
    stream.expect(TokenKind::CDStart)?;
    if let TokenKind::CharData(_char_data) = stream.current() {
        stream.advance();
    }
    stream.expect(TokenKind::CDEnd)?;
    Ok(())
}

/// [22] prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if let TokenKind::XmlDeclStart = stream.current() {
        parse_xml_decl(stream, ctx)?;
    }

    parse_misc(stream)?;

    if stream.current() == TokenKind::DTDStart {
        parse_doc_type_decl(stream, ctx)?;
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
fn parse_xml_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::XmlDeclStart)?;
    stream.expect_whitespace("before document version")?;
    stream.expect(TokenKind::Version)?;
    stream.expect(TokenKind::Equal)?;

    let version = stream.expect_and_get_literal()?;
    ctx.set_version(version);

    stream.consume_whitespace();
    if let TokenKind::Encoding = stream.current() {
        stream.expect_preceeding_whitespace("encoding")?;
        stream.advance();
        stream.expect(TokenKind::Equal)?;

        let encoding = stream.expect_and_get_literal()?;
        ctx.set_encoding(encoding);
    }

    stream.consume_whitespace();
    if let TokenKind::Standalone = stream.current() {
        stream.expect_preceeding_whitespace("standalone")?;
        stream.advance();
        stream.expect(TokenKind::Equal)?;

        let standalone = stream.expect_and_get_literal()?;
        ctx.set_standalone(standalone);
    }

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
fn parse_doc_type_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("before DTD name")?;

    let name = stream.expect_and_get_name()?;
    ctx.initialize_dtd(name);

    // TODO: add entity
    let (_system_id, _public_id) = parse_external_id(stream, false)?;

    stream.consume_whitespace();
    if stream.is(TokenKind::IntSubsetStart) {
        stream.advance();

        // parse 'intSubset'
        loop {
            match stream.current() {
                TokenKind::IntSubsetEnd => break,
                TokenKind::ElementDecl => parse_element_type_decl(stream)?,
                TokenKind::EntityDecl => parse_entity_decl(stream, ctx)?,
                TokenKind::AttlistDecl => parse_attlist_decl(stream, ctx)?,
                TokenKind::NotationDecl => parse_notation_decl(stream, ctx)?,
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

/// [39] element       ::=  EmptyElemTag | STag content ETag
/// [40] STag          ::=  '<' QName (S Attribute)* S? '>'
/// [42] ETag          ::=  '</' Name S? '>'
/// [44] EmptyElemTag  ::=  '<' QName (S Attribute)* S? '/>'
fn parse_element<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::OpenTagStart)?;
    let _name = stream.expect_and_get_name()?;

    loop {
        match stream.current() {
            TokenKind::EmptyTagEnd => {
                stream.advance();
                return Ok(());
            }
            TokenKind::TagEnd => {
                stream.advance();
                break;
            }
            TokenKind::Name(_name) => {
                // TODO: Something with attribute
                stream.expect_preceeding_whitespace("attribute name")?;
                stream.advance();
                stream.expect(TokenKind::Equal)?;
                let _att_value = stream.expect_and_get_literal()?;
            }
            TokenKind::Whitespace(_) => stream.advance(),
            kind => panic!("unexpected token while parsing element: {kind:?}"),
        }
    }

    parse_content(stream)?;

    stream.expect(TokenKind::TagEndStart)?;
    let _name = stream.expect_and_get_name()?;
    stream.consume_whitespace();
    stream.expect(TokenKind::TagEnd)?;

    Ok(())
}

/// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    loop {
        match stream.current() {
            TokenKind::TagEndStart => break,
            TokenKind::Whitespace(_) => stream.advance(),
            TokenKind::PIStart => parse_processing_instruction(stream)?,
            TokenKind::CDStart => parse_cdata(stream)?,
            TokenKind::Comment(_) => parse_comment(stream)?,
            TokenKind::OpenTagStart => parse_element(stream)?,
            TokenKind::CharData(_char_data) => stream.advance(),
            TokenKind::Ampersand => parse_reference(stream)?,
            kind => panic!("unexpected token while parsing content: {kind:?}"),
        }
    }

    stream.consume_whitespace();

    Ok(())
}

/// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
/// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("before ElementDecl Name")?;

    let _name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after ElementDecl Name")?;

    match stream.current() {
        TokenKind::Empty => {
            // TODO
            stream.advance();
        }
        TokenKind::Any => {
            // TODO
            stream.advance();
        }
        TokenKind::LeftParen => {
            stream.advance();
            stream.consume_whitespace();
            if let TokenKind::PCData = stream.current() {
                stream.advance();
                parse_mixed(stream)?;
            } else {
                parse_element_content_children(stream)?;
            }
        }
        kind => panic!("expected [Empty | Any | '('], got {kind:?}"),
    }

    stream.expect(TokenKind::MarkupDeclEnd)?;

    Ok(())
}

/// [47] children  ::=   (choice | seq) ('?' | '*' | '+')?
/// [48] cp        ::=   (Name | choice | seq) ('?' | '*' | '+')?
/// [49] choice    ::=   '(' S? cp ( S? '|' S? cp )+ S? ')'
/// [50] seq       ::=   '(' S? cp ( S? ',' S? cp )* S? ')'
fn parse_element_content_children<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // TODO: Return Repetition
    fn parse_repetition<'a>(stream: &mut TokenStream<'a>) -> () {
        match stream.current() {
            TokenKind::QuestionMark => {
                stream.advance();
                ()
            }
            TokenKind::Star => {
                stream.advance();
                ()
            }
            TokenKind::Plus => {
                stream.advance();
                ()
            }
            _ => (),
        }
    }

    // TODO: Return ContentParticle
    fn parse_content_particle<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
        match stream.current() {
            TokenKind::Percent => {
                stream.advance();
                stream.expect_and_get_name()?;
                stream.expect(TokenKind::SemiColon)?;
                panic!("illegal parameter entity reference in content particle")
            }
            TokenKind::LeftParen => {
                stream.advance();
                stream.consume_whitespace();
                let _children = parse_element_content_children(stream)?;
                let _repetition = parse_repetition(stream);
            }
            TokenKind::Name(_name) => {
                stream.advance();
                stream.consume_whitespace();
                let _repetition = parse_repetition(stream);
            }
            kind => panic!("invalid content particle [{kind:?}]"),
        }
        Ok(())
    }

    enum Type {
        Seq,
        Choice,
    }

    // Leading '(' and any whitespace was already consumed
    parse_content_particle(stream)?;

    let mut content_type: Option<Type> = None;
    let mut expecting_content = false;

    loop {
        match stream.current() {
            TokenKind::Percent => {
                stream.advance();
                stream.expect_and_get_name()?;
                stream.expect(TokenKind::SemiColon)?;
                panic!("illegal parameter entity reference in element content decl")
            }
            TokenKind::RightParen => {
                if expecting_content {
                    panic!("expected element content, got ')'")
                }

                stream.advance();
                break;
            }
            TokenKind::Comma => {
                if expecting_content {
                    panic!("expected element content, got ','")
                }

                match content_type {
                    None => content_type = Some(Type::Seq),
                    Some(Type::Choice) => {
                        panic!("invalid content separator ','")
                    }
                    Some(Type::Seq) => {}
                }

                stream.advance();
                stream.consume_whitespace();
                expecting_content = true;
            }
            TokenKind::Pipe => {
                if expecting_content {
                    panic!("expected element content, got ','")
                }

                match content_type {
                    None => content_type = Some(Type::Choice),
                    Some(Type::Seq) => {
                        panic!("invalid content separator '|'")
                    }
                    Some(Type::Choice) => {}
                }

                stream.advance();
                stream.consume_whitespace();
                expecting_content = true;
            }
            TokenKind::LeftParen => {
                if !expecting_content {
                    panic!("unexpected token in left paren [{:?}]", stream.current());
                }

                stream.advance();
                stream.consume_whitespace();
                let _children = parse_element_content_children(stream)?;
                let _repetition = parse_repetition(stream);
                expecting_content = false;
            }
            TokenKind::Name(_name) => {
                if !expecting_content {
                    panic!("unexpected token in name [{:?}]", stream.current());
                }

                stream.advance();
                stream.consume_whitespace();
                let _repetition = parse_repetition(stream);
                expecting_content = false;
            }
            kind => panic!("unexpected token [{kind:?}]"),
        }
    }

    Ok(())
}

/// [51] Mixed  ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
fn parse_mixed<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // '(' and whitespace already consumed

    let mut names = vec![];
    loop {
        stream.consume_whitespace();

        match stream.current() {
            TokenKind::RightParen => {
                stream.advance();
                if !names.is_empty() {
                    stream.expect(TokenKind::Star)?;
                }
                break;
            }
            TokenKind::Pipe => {
                stream.advance();
                stream.consume_whitespace();
                names.push(stream.expect_and_get_name()?);
            }
            kind => panic!("unexpected token in Mixed Content Decl: {kind:?}"),
        }
    }

    // TODO: Emit MixedContent
    Ok(())
}

/// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
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

/// [66]  CharRef  ::=  &#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
/// [67]  Reference  ::=  EntityRef | CharRef
/// [68]  EntityRef  ::=  '&' Name ';'
fn parse_reference<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    stream.advance();

    match stream.current() {
        TokenKind::Pound => {
            stream.advance();

            if let TokenKind::ReferenceValue(val) = stream.current() {
                stream.advance();
                if val.starts_with("x") {
                    // TODO: Validate hexadecimal
                } else {
                    // TODO: Validate decimal
                }
            } else {
                panic!("unexpected token while parsing CharRef: {:?}", stream.current())
            }
        }
        TokenKind::ReferenceValue(val) => {
            validate::is_valid_name2(val)?;
            stream.advance();
        }
        kind => panic!("unexpected token while parsing reference: {kind:?}"),
    }

    stream.expect(TokenKind::SemiColon)?;

    Ok(())
}

fn parse_entity_ref<'a>(stream: &mut TokenStream<'a>) -> ParseResult<()> {
    // lexer parses everything between the '&' and ';' as a 'ReferenceValue' so we have
    // to ensure we validiate here
    stream.expect(TokenKind::Ampersand)?;
    let _name = stream.expect_and_get_name()?;
    stream.expect(TokenKind::SemiColon)?;
    Ok(())
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
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.expect(TokenKind::EntityDecl)?;
    stream.expect_whitespace("after EntityDecl")?;

    if let TokenKind::Percent = stream.current() {
        parse_parameter_entity_decl(stream, ctx)?;
    } else {
        parse_general_entity_decl(stream, ctx)?;
    }

    Ok(())
}

/// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
/// [73] EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn parse_general_entity_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    let name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after EntityDecl name")?;

    let entity_type = {
        if let TokenKind::Literal(value) = stream.current() {
            stream.advance();
            EntityType::InternalGeneral { value }
        } else {
            let (system_id, public_id) = parse_external_id(stream, true)?;

            stream.consume_whitespace();

            if let TokenKind::NData = stream.current() {
                let ndata = parse_ndata_decl(stream)?;
                EntityType::ExternalGeneralUnparsed {
                    system_id,
                    public_id,
                    ndata,
                }
            } else {
                EntityType::ExternalGeneralParsed { system_id, public_id }
            }
        }
    };

    stream.consume_whitespace();
    stream.expect(TokenKind::MarkupDeclEnd)?;

    ctx.emit_entity_decl(name, entity_type);

    Ok(())
}

/// [72] PEDecl  ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
/// [74] PEDef   ::=  EntityValue | ExternalID
fn parse_parameter_entity_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("before PEDecl name")?;
    let _name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after PEDecl name")?;

    let entity_type = {
        if let TokenKind::Literal(value) = stream.current() {
            stream.advance();
            EntityType::InternalParameter { value }
        } else {
            let (system_id, public_id) = parse_external_id(stream, true)?;
            EntityType::ExternalParameter { system_id, public_id }
        }
    };

    stream.consume_whitespace();
    stream.expect(TokenKind::MarkupDeclEnd)?;

    Ok(())
}

/// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(
    stream: &mut TokenStream<'a>,
    required: bool,
) -> ParseResult<(Option<&'a str>, Option<&'a str>)> {
    stream.consume_whitespace();

    if stream.is(TokenKind::System) {
        stream.advance();
        stream.expect_whitespace("ExternalID SystemLiteral")?;
        let system_id = stream.expect_and_get_literal()?;
        return Ok((Some(system_id), None));
    }

    if stream.is(TokenKind::Public) {
        stream.advance();
        stream.expect_whitespace("ExternalId PubidLiteral")?;
        let public_id = stream.expect_and_get_literal()?;
        stream.expect_whitespace("ExternalID SystemLiteral")?;
        let system_id = stream.expect_and_get_literal()?;

        return Ok((Some(system_id), Some(public_id)));
    }

    if required {
        panic!("missing required external id")
    } else {
        Ok((None, None))
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
fn parse_notation_decl<'a>(stream: &mut TokenStream<'a>, ctx: &mut Context<'a>) -> ParseResult<()> {
    stream.advance();
    stream.expect_whitespace("after <!NOTATION")?;
    let name = stream.expect_and_get_name()?;
    stream.expect_whitespace("after NotationDecl name")?;

    // Three paths we can take
    // 1: SYSTEM SystemLiteral
    // 2: PUBLIC PubidLiteral
    // 3: PUBLIC PubidLiteral SystemLiteral
    match stream.current() {
        TokenKind::System => {
            stream.advance();
            stream.expect_whitespace("After SYSTEM in NotationDecl")?;

            let system_id = stream.expect_and_get_literal()?;
            ctx.emit_notation_decl(name, None, Some(system_id));
        }
        TokenKind::Public => {
            stream.advance();
            stream.expect_whitespace("After PUBLIC in NotationDecl")?;
            let public_id = Some(stream.expect_and_get_literal()?);

            let system_id = {
                stream.consume_whitespace();
                if let TokenKind::Literal(system_id) = stream.current() {
                    stream.expect_preceeding_whitespace("System Literal in Notation Decl")?;
                    Some(system_id)
                } else {
                    None
                }
            };

            ctx.emit_notation_decl(name, public_id, system_id);
        }
        kind => panic!("expected ExternalId or PublicId in NotationDecl, got {kind:?}"),
    }

    stream.consume_whitespace();
    stream.expect(TokenKind::MarkupDeclEnd)?;

    Ok(())
}
