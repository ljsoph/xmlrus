use xmlrus::ParseError;
use xmlrus::Parser;

fn source(test_file: &str) -> String {
    std::fs::read_to_string(format!("tests/test_files/{test_file}.xml")).unwrap_or_default()
}

#[test]
fn test_unclosed_root() {
    let source = source("unclosed_root");
    let res = xmlrus::Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::UnclosedRoot)))
}

#[test]
fn test_unclosed_root_2() {
    let source = source("unclosed_root_2");
    let res = xmlrus::Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::UnexpectedCharacter(_, _, _))))
}

#[test]
fn declaration_ok() {
    let source = source("declaration_ok");
    let res = Parser::parse(&source);
    assert!(res.is_ok());
}

#[test]
fn declaration_ok_2() {
    let source = source("declaration_ok_2");
    let res = Parser::parse(&source);
    assert!(res.is_ok());
}

#[test]
fn declaration_ok_standalone() {
    let source = source("declaration_ok_standalone");
    let res = Parser::parse(&source);
    assert!(res.is_ok());
}

#[test]
fn declaration_invalid_standalone() {
    let source = source("declaration_invalid_standalone");
    let res = Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::InvalidStandalone(_, _))))
}

#[test]
fn declaration_missing_version() {
    let source = source("declaration_missing_version");
    let res = Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::InvalidDeclaration(_, _))))
}

#[test]
fn unclosed_declaration() {
    let source = source("declaration_unclosed");
    let res = Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::InvalidDeclaration(_, _))));
}

#[test]
fn duplicate_declaration() {
    let source = source("declaration_duplicate");
    let res = Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::UnexpectedDeclaration(_))));
}

#[test]
fn declaration_not_first() {
    let source = source("declaration_not_first");
    let res = Parser::parse(&source);
    assert!(matches!(res, Err(ParseError::UnexpectedDeclaration(_))));
}
