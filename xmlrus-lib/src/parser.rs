use crate::lexer::Token;
use crate::lexer::TokenKind;

struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
    pos: usize,
}

impl<'a> TokenStream<'a> {
    fn current(&self) -> TokenKind<'a> {
        self.tokens[self.pos].kind
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, kind: TokenKind) {
        let current = self.current();
        if current != kind {
            panic!("Expected {kind:?}, got {current:?}")
        }

        self.advance();
    }

    fn expect_whitespace(&mut self) {
        if let TokenKind::Whitespace(_) = self.current() {
            self.advance();
            return;
        }

        panic!("Expected whitespace")
    }

    fn expect_and_get_literal(&mut self) -> &'a str {
        match self.current() {
            TokenKind::Literal(literal) => {
                self.advance();
                return literal;
            }
            _ => panic!("expected literal"),
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
fn parse_document<'a>(stream: &mut TokenStream<'a>) {
    parse_prolog(stream);
}

/// [9] EntityValue  ::=  '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
fn parse_entity_value<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [10]  AttValue  ::=  '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
fn parse_attribute_value<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [11] SystemLiteral  ::=  ('"' [^"]* '"') | ("'" [^']* "'")
fn parse_system_literal<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
/// [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn parse_public_id_literal<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [14]  CharData  ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
fn parse_chardata<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [15]  Comment  ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
fn parse_comment<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [16] PI        ::=   '<?' PITarget  (S (Char* - ( Char* '?>' Char*)))? '?>'
/// [17] PITarget  ::=   Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_processing_instruction<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [18] CDSect   ::=  CDStart CData CDEnd
/// [19] CDStart  ::=  '<![CDATA['
/// [20] CData    ::=  (Char* - (Char* ']]>' Char*))
/// [21] CDEnd    ::=  ']]>'
fn parse_cdata<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [22] prolog  ::=  XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog<'a>(stream: &mut TokenStream<'a>) {
    // There can only be one XML declaration, and it must be at the absolute start
    // of the Document, i.e., no characters are allowed before it (including whitespace)
    if let TokenKind::XmlDeclStart = stream.current() {
        parse_xml_decl(stream)
    }
}

/// [23] XMLDecl       ::=   '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
/// [24] VersionInfo   ::=   S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
/// [25] Eq            ::=   S? '=' S?
/// [26] VersionNum    ::=   '1.' [0-9]+
/// [80] EncodingDecl  ::=   S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
/// [81] EncName       ::=   [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
/// [32] SDDecl        ::=   S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn parse_xml_decl<'a>(stream: &mut TokenStream<'a>) {
    stream.expect(TokenKind::XmlDeclStart);
    stream.expect_whitespace();
    stream.expect(TokenKind::Name("version"));
    stream.expect(TokenKind::Equal);

    let version = stream.expect_and_get_literal();
    println!("Version: {version}");
}

/// [27] Misc  ::=  Comment | PI | S
fn parse_misc<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [28]  doctypedecl ::=   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
/// [28a] DeclSep     ::=   PEReference | S
/// [28b] intSubset   ::=   (markupdecl | DeclSep)*
/// [29]  markupdecl  ::=   elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
fn parse_doc_type_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [39] element  ::=  EmptyElemTag | STag content ETag
fn parse_element<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [40] STag          ::= '<' QName (S Attribute)* S? '>'
/// [44] EmptyElemTag  ::= '<' QName (S Attribute)* S? '/>'
fn parse_element_start<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [42]  ETag  ::=  '</' Name S? '>'
fn parse_etag<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [43] content  ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
fn parse_content<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [45] elementdecl  ::=  '<!ELEMENT' S Name S contentspec S? '>'
/// [46] contentspec  ::=  'EMPTY' | 'ANY' | Mixed | children
fn parse_element_type_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [52] AttlistDecl  ::=  '<!ATTLIST' S Name AttDef* S? '>'
fn parse_attlist_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [53] AttDef         ::=  S Name S AttType S DefaultDecl
fn parse_att_def<'a>(stream: &mut TokenStream<'a>) {
    // TODO
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
fn parse_att_type<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [57]  EnumeratedType  ::=  NotationType | Enumeration
fn parse_enumerated_type<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [58]  NotationType  ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
fn parse_notation_type<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [59]  Enumeration  ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
fn parse_enumeration<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [70] EntityDecl  ::=  GEDecl | PEDecl
/// [71] GEDecl      ::=  '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl      ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'
fn parse_entity_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [73] EntityDef   ::=  EntityValue | (ExternalID NDataDecl?)
fn parse_entity_def<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [74]  PEDef  ::=  EntityValue | ExternalID
fn parse_pe_def<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [75] ExternalID  ::=  'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
fn parse_external_id<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [76] NDataDecl  ::=  S 'NDATA' S Name
fn parse_ndata_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}

/// [82] NotationDecl  ::=  '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
/// [83] PublicID      ::=  'PUBLIC' S PubidLiteral
fn parse_notation_decl<'a>(stream: &mut TokenStream<'a>) {
    // TODO
}
