use super::tokens::{Token, LiteralType, reserved_to_tokentype};
use anyhow::Result;
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    line: usize,
}

#[derive(Error, Debug)]
enum LexError {
    #[error("Unexpected token {found:?} found on line {line:?}.")]
    UnexpectedToken { found: char, line: usize },
    #[error("Unterminated string.")]
    UnterminatedString,
}

impl<'source> Lexer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self, source: &mut Peekable<Chars>) -> Result<()> {
        while let Some(c) = source.next() {
            match c {
                // Single-character tokens
                '(' => self.add_token(Token::LeftParen),
                ')' => self.add_token(Token::RightParen),
                '{' => self.add_token(Token::LeftBrace),
                '}' => self.add_token(Token::RightBrace),
                ',' => self.add_token(Token::Comma),
                '.' => self.add_token(Token::Dot),
                '-' => self.add_token(Token::Minus),
                '+' => self.add_token(Token::Plus),
                '*' => self.add_token(Token::Star),
                ';' => self.add_token(Token::Semicolon),
                // Two-character tokens
                '!' => {
                    if let Some(_) = source.next_if(|c| *c == '=') {
                        self.add_token(Token::BangEqual);
                    } else {
                        self.add_token(Token::Bang)
                    }
                },
                '=' => {
                    if let Some(_) = source.next_if(|c| *c == '=') {
                        self.add_token(Token::EqualEqual);
                    } else {
                        self.add_token(Token::Equal);
                    }
                },
                '<' => {
                    if let Some(_) = source.next_if(|c| *c == '=') {
                        self.add_token(Token::LessEqual);
                    } else {
                        self.add_token(Token::Less);
                    }
                },
                '>' => {
                    if let Some(_) = source.next_if(|c| *c == '=') {
                        self.add_token(Token::GreaterEqual);
                    } else {
                        self.add_token(Token::Greater);
                    }
                },
                // Comments
                '/' => {
                    if let Some('/') = source.peek() {
                        // Skip comment line
                        while let Some(ch) = source.next() {
                            if ch == '\n' {
                                self.line += 1;
                                break;
                            }
                        }
                    } else {
                        self.add_token(Token::Slash);
                    }
                },
                // String literals
                '"' => {
                    let mut s = String::new();
                    loop {
                        if let Some(ch) = source.next() {
                            if ch == '"' {
                                break;
                            }
                            if ch != '\n' {
                                s.push(ch);
                            } else {
                                // Multi-line string
                                self.line += 1;
                            }
                        } else {
                            // Unterminated string
                            anyhow::bail!(LexError::UnterminatedString);
                        }
                    }
                    self.add_token(Token::Literal(LiteralType::String(s)));
                },
                // Numeric literals
                '0'..='9' => {
                    let mut number_literal = String::new();
                    number_literal.push(c);
                    loop {
                        if let Some(ch) = source.peek() {
                            if ch.is_ascii_digit() || *ch == '.' {
                                number_literal.push(*ch);
                                source.next();
                            } else {
                                break;
                            }
                        }
                    }
                    let number: f32 = number_literal.parse()?;
                    self.add_token(Token::Literal(LiteralType::Number(number)));
                }
                // Identifiers
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut id = String::new();
                    id.push(c);
                    loop {
                        if let Some(ch) = source.peek() {
                            if ch.is_ascii_alphanumeric() || *ch == '_' {
                                id.push(*ch);
                                source.next();
                            } else {
                                break;
                            }
                        }
                    }
                    // Check against keywords
                    if let Some(keyword) = reserved_to_tokentype(&id) {
                        self.add_token(keyword);
                    } else {
                        // Just an identifier
                        self.add_token(Token::Literal(LiteralType::Identifier(id)));
                    }
                }

                // Whitespace
                ' ' | '\t' | '\r' => {}
                '\n' => self.line += 1,
                _ => anyhow::bail!(LexError::UnexpectedToken {
                    found: c,
                    line: self.line
                }),
            }
        }
        self.add_token(Token::EOF);
        println!("Tokens: {:?}", self.tokens);
        Ok(())
    }

    fn add_token(&mut self, token_type: Token) {
        self.tokens.push(token_type);
    }
}
