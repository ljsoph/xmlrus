use std::{fs, vec::IntoIter};

use anyhow::{Result, anyhow};

#[derive(Clone, Debug, PartialEq)]
enum ElementEndKind {
    /// Closing tag of the begining of a non-empty element `>`
    Open,
    /// Closing tag of a non-emtpy element `</tagname>`
    NonEmpty(String),
    /// Closing tag of an empty element `/>`
    Empty,
}

#[derive(Debug, PartialEq)]
enum Token {
    Prolog,
    ElementStart { id: String },
    ElementEnd { kind: ElementEndKind },
    Attribute { key: String, value: String },
}

struct Lexer {
    source: Vec<char>,
    len: usize,
    current: usize,
}

impl Lexer {
    fn new(source: String) -> Self {
        let source: Vec<char> = source.chars().collect();
        let len = source.len();

        Self {
            source,
            len,
            current: 0,
        }
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
        self.collect()
    }

    /// Advances the iterator and returns the last (current) value
    fn advance(&mut self) -> Option<char> {
        if self.current >= self.len {
            return None;
        }
        self.current += 1;
        Some(self.source[self.current - 1])
    }

    fn peek(&self) -> Option<char> {
        if self.current >= self.len {
            None
        } else {
            Some(self.source[self.current])
        }
    }

    fn lex_prolog(&mut self) -> Option<Token> {
        self.advance();
        while let Some(c) = self.advance() {
            if c == '?' {
                if let Some(next_char) = self.peek() {
                    if next_char == '>' {
                        self.advance();
                        return Some(Token::Prolog);
                    }
                }
            }
        }
        None
    }

    fn lex_element_start(&mut self) -> Option<Token> {
        let current_char = self.advance()?;
        let mut string = String::from(current_char);

        while let Some(c) = self.peek() {
            // Bad!
            if c == '\n' || c == '\r' {
                return None;
            }

            // Child Element
            if c == ' ' || c == '>' {
                self.advance();
                break;
            }

            string.push(self.advance().expect("Invalid next char"));
        }

        Some(Token::ElementStart { id: string })
    }

    fn lex_element_end(&mut self) -> Option<Token> {
        // skip the '/' in the closing tag
        self.advance()?;
        let current_char = self.advance()?;
        let mut string = String::from(current_char);

        while let Some(c) = self.peek() {
            // Bad!
            if c == '\n' || c == '\r' {
                return None;
            }
            if c == '>' {
                self.advance();
                break;
            }

            string.push(self.advance().expect("Invalid next char"));
        }

        Some(Token::ElementEnd {
            kind: ElementEndKind::NonEmpty(string),
        })
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.advance() {
            None => None,
            Some(' ' | '\n' | '\t' | '\r') => self.next(),
            Some('<') => {
                if let Some(next_char) = self.peek() {
                    match next_char {
                        // Start of the Prolog
                        '?' => self.lex_prolog(),
                        // Root Element closing tag
                        '/' => self.lex_element_end(),
                        // Start of a Child element
                        _ => self.lex_element_start(),
                    }
                } else {
                    None
                }
            }
            Some('/') => match self.peek() {
                Some('>') => {
                    self.advance();
                    Some(Token::ElementEnd {
                        kind: ElementEndKind::Empty,
                    })
                }
                Some(_) | None => None,
            },
            Some('>') => Some(Token::ElementEnd {
                kind: ElementEndKind::Open,
            }),
            Some(current_char) => {
                let mut key = String::from(current_char);
                let mut value = String::new();

                while let Some(c) = self.peek() {
                    // Bad!
                    if c.is_ascii_whitespace() {
                        return None;
                    }

                    if c == '=' {
                        self.advance();
                        break;
                    }

                    key.push(self.advance().expect("Invalid next char"));
                }

                let next_char = self.advance()?;
                if next_char != '"' {
                    return None;
                }

                while let Some(c) = self.peek() {
                    // Bad!
                    if c == '\r' || c == '\n' {
                        return None;
                    }

                    if c == '"' {
                        self.advance();
                        break;
                    }

                    value.push(self.advance().expect("Invalid next char"));
                }

                Some(Token::Attribute { key, value })
            }
        }
    }
}

#[derive(Debug)]
struct Element {
    id: String,
    attributes: Vec<Attribute>,
    children: Vec<Element>,
}

#[derive(Debug)]
struct Attribute {
    key: String,
    value: String,
}

struct Parser;

impl Parser {
    /// Entry point to the parser.
    ///
    /// Parses each element inside the `root` element.
    fn parse(tokens: Vec<Token>) -> Result<Vec<Element>> {
        if tokens.is_empty() {
            return Err(anyhow!(
                "parse: Failed to parse, empty token list supplied!"
            ));
        }

        // We don't have Prolog so the first element is the root.
        let has_prolog = matches!(tokens.first(), Some(Token::Prolog));
        let mut elements: Vec<Element> = Vec::new();
        let mut tokens = tokens.into_iter();

        if has_prolog {
            // Consume the Prolog, we don't care about it.
            tokens.next();
        }

        if tokens.next().is_none() {
            return Err(anyhow!(
                "Unexpected EOF when parsing Root Element opening tag"
            ));
        }

        while let Some(token) = tokens.next() {
            match token {
                Token::ElementStart { id } => elements.push(Self::parse_element(&id, &mut tokens)),
                // This __should__ be the end of the root element.
                // Trust 🙏
                Token::ElementEnd { .. } => {}
                Token::Prolog | Token::Attribute { key: _, value: _ } => {
                    return Err(anyhow!(
                        "parse: Encountered invalid token - [Token::{token:?}]"
                    ));
                }
            }
        }

        Ok(elements)
    }

    /// Recursively parse an element (including all child elements)
    fn parse_element(id: &str, tokens: &mut IntoIter<Token>) -> Element {
        let mut attributes = Vec::new();
        let mut children = Vec::new();

        while let Some(token) = tokens.next() {
            match &token {
                Token::Attribute { key, value } => attributes.push(Attribute {
                    key: key.to_string(),
                    value: value.to_string(),
                }),
                Token::ElementStart { id } => children.push(Self::parse_element(id, tokens)),
                Token::ElementEnd {
                    kind: ElementEndKind::Empty | ElementEndKind::NonEmpty(_),
                } => break,
                // This is the closing tag of the start of a non-empty element, we will need to
                // recursively parse the children elements so we do not want to break here.
                Token::ElementEnd { .. } | Token::Prolog => {}
            }
        }

        Element {
            id: id.to_string(),
            attributes,
            children,
        }
    }
}

#[derive(Debug, Default)]
struct GitRemote {
    name: String,
    url: String,
}

#[derive(Debug, Default)]
struct GitProject {
    name: String,
    remote: String,
    revision: String,
    path: String,
}

fn main() {
    // let source = fs::read_to_string("test.xml").unwrap_or_default();
    let source = fs::read_to_string("test_two.xml").unwrap_or_default();
    let mut lexer = Lexer::new(source);
    let tokens = lexer.scan_tokens();

    for token in &tokens {
        println!("{token:?}");
    }

    let elements = match Parser::parse(tokens) {
        Ok(elements) => elements,
        Err(err) => {
            eprintln!("Failed to parse tokens: {err}");
            return;
        }
    };

    // for element in &elements {
    //     println!("{element:#?}");
    // }

    let remotes = elements
        .iter()
        .filter_map(|e| {
            if e.id == *"remote".to_string() {
                let mut remote = GitRemote::default();
                for attribute in &e.attributes {
                    match attribute.key.as_str() {
                        "name" => remote.name.clone_from(&attribute.value),
                        "url" => remote.url.clone_from(&attribute.value),
                        _ => (),
                    }
                }
                Some(remote)
            } else {
                None
            }
        })
        .collect::<Vec<GitRemote>>();

    println!("{remotes:#?}");

    let projects = elements
        .iter()
        .filter_map(|e| {
            if e.id == *"project".to_string() {
                let mut project = GitProject::default();
                for attribute in &e.attributes {
                    match attribute.key.as_str() {
                        "name" => project.name.clone_from(&attribute.value),
                        "remote" => project.remote.clone_from(&attribute.value),
                        "revision" => project.revision.clone_from(&attribute.value),
                        "path" => project.path.clone_from(&attribute.value),
                        _ => (),
                    }
                }
                Some(project)
            } else {
                None
            }
        })
        .collect::<Vec<GitProject>>();
    println!("{projects:#?}");
}
