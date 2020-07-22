use super::tokens::{Token, reserved_to_tokentype, LiteralType, TokenType};
use anyhow::Result;
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    line: usize,
}

#[derive(Error, Debug, PartialEq)]
enum LexError {
    #[error("Unexpected token {found:?} found on line {line:?}.")]
    UnexpectedToken { found: char, line: usize },
    #[error("Unterminated string.")]
    UnterminatedString,
    #[error("Invalid number format. No digit provided after decimal point?")]
    InvalidNumberFormat,
}

impl<'source> Lexer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn get_tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn scan_tokens(&mut self, source: &mut Peekable<Chars>) -> Result<()> {
        while let Some(c) = source.next() {
            match c {
                // Single-character tokens
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                ',' => self.add_token(TokenType::Comma),
                '.' => self.add_token(TokenType::Dot),
                '-' => self.add_token(TokenType::Minus),
                '+' => self.add_token(TokenType::Plus),
                '*' => self.add_token(TokenType::Star),
                ';' => self.add_token(TokenType::Semicolon),
                // Two-character tokens
                '!' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(TokenType::BangEqual);
                    } else {
                        self.add_token(TokenType::Bang)
                    }
                }
                '=' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(TokenType::EqualEqual);
                    } else {
                        self.add_token(TokenType::Equal);
                    }
                }
                '<' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(TokenType::LessEqual);
                    } else {
                        self.add_token(TokenType::Less);
                    }
                }
                '>' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(TokenType::GreaterEqual);
                    } else {
                        self.add_token(TokenType::Greater);
                    }
                }
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
                        self.add_token(TokenType::Slash);
                    }
                }
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
                    self.add_token(TokenType::Literal(LiteralType::String(s)));
                }
                // Numeric literals
                '0'..='9' => {
                    let mut number_literal = String::new();
                    number_literal.push(c);
                    while let Some(ch) = source.peek() {
                        if ch.is_ascii_digit() {
                            number_literal.push(*ch);
                            source.next();
                        } else if *ch == '.' {
                            // Push the decimal point and go to next char
                            number_literal.push(*ch);
                            source.next();
                            // Make sure number after decimal is digit
                            if let Some(digit) = source.peek() {
                                if !digit.is_ascii_digit() {
                                    anyhow::bail!(LexError::InvalidNumberFormat);
                                }
                            } else {
                                anyhow::bail!(LexError::InvalidNumberFormat);
                            }
                        } else {
                            // Escape this nightmare
                            break;
                        }
                    }
                    let number: f32 = number_literal.parse()?;
                    self.add_token(TokenType::Literal(LiteralType::Number(number)));
                }
                // Identifiers
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut id = String::new();
                    id.push(c);

                    while let Some(ch) = source.peek() {
                        if ch.is_ascii_alphanumeric() || *ch == '_' {
                            id.push(*ch);
                            source.next();
                        } else {
                            break;
                        }
                    }
                    // Check against keywords
                    if let Some(keyword) = reserved_to_tokentype(&id) {
                        self.add_token(keyword);
                    } else {
                        // Just an identifier
                        self.add_token(TokenType::Literal(LiteralType::Identifier(id)));
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
        self.add_token(TokenType::EOF);
        // println!("Tokens: {:?}", self.tokens);
        Ok(())
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(token_type, self.line));
    }
}

/// Tests
/// Note: all tests omit EOF
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_tokens() {
        let mut lexer = Lexer::new();
        let input = "(){},.-+*;";
        let expected: [Token; 10] = [
            Token::new( TokenType::LeftParen, 1),
            Token::new( TokenType::RightParen, 1),
            Token::new( TokenType::LeftBrace, 1),
            Token::new( TokenType::RightBrace, 1),
            Token::new( TokenType::Comma, 1),
            Token::new( TokenType::Dot, 1),
            Token::new( TokenType::Minus, 1),
            Token::new( TokenType::Plus, 1),
            Token::new( TokenType::Star, 1),
            Token::new( TokenType::Semicolon, 1),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(actual) = token_iter.next() {
                assert_eq!(exp.token, actual.token);
            } else {
                panic!();
            }
        }
    }

    #[test]
    fn test_double_tokens() {
        let mut lexer = Lexer::new();
        let input = "! != = == > >= < <=";
        let expected: [Token; 8] = [
            Token::new(TokenType::Bang,1),
            Token::new(TokenType::BangEqual,1),
            Token::new(TokenType::Equal,1),
            Token::new(TokenType::EqualEqual,1),
            Token::new(TokenType::Greater,1),
            Token::new(TokenType::GreaterEqual,1),
            Token::new(TokenType::Less,1),
            Token::new(TokenType::LessEqual,1),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(actual) = token_iter.next() {
                assert_eq!(exp.token, actual.token);
            } else {
                panic!();
            }
        }
    }

    #[test]
    fn test_literal_tokens() {
        let mut lexer = Lexer::new();
        let input = r#"Test_Class _unused "my string" 0.1 123 123.45"#;
        let expected: [Token; 6] = [
            Token::new(TokenType::Literal(LiteralType::Identifier("Test_Class".to_string())),1),
            Token::new(TokenType::Literal(LiteralType::Identifier("_unused".to_string())),1),
            Token::new(TokenType::Literal(LiteralType::String("my string".to_string())),1),
            Token::new(TokenType::Literal(LiteralType::Number(0.1)),1),
            Token::new(TokenType::Literal(LiteralType::Number(123.)),1),
            Token::new(TokenType::Literal(LiteralType::Number(123.45)),1),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp.token, token.token);
            } else {
                panic!()
            }
        }
    }

    #[test]
    fn test_reserved_tokens() {
        let mut lexer = Lexer::new();
        let input = "and class else false fun for if nil or print return super this true var while";
        let expected: [Token; 16] = [
            Token::new(TokenType::And,1),
            Token::new(TokenType::Class,1),
            Token::new(TokenType::Else,1),
            Token::new(TokenType::False,1),
            Token::new(TokenType::Fun,1),
            Token::new(TokenType::For,1),
            Token::new(TokenType::If,1),
            Token::new(TokenType::Nil,1),
            Token::new(TokenType::Or,1),
            Token::new(TokenType::Print,1),
            Token::new(TokenType::Return,1),
            Token::new(TokenType::Super,1),
            Token::new(TokenType::This,1),
            Token::new(TokenType::True,1),
            Token::new(TokenType::Var,1),
            Token::new(TokenType::While,1),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp.token, token.token);
            } else {
                panic!()
            }
        }
    }

    #[test]
    fn test_mixed_tokens() {
        let mut lexer = Lexer::new();
        let input = r#"
        if(i == 6)
        { 
            print "hey mom"; 
        }
        "#;
        let expected: [Token; 11] = [
            Token::new(TokenType::If,1),
            Token::new(TokenType::LeftParen,1),
            Token::new(TokenType::Literal(LiteralType::Identifier("i".to_string())),1),
            Token::new(TokenType::EqualEqual,1),
            Token::new(TokenType::Literal(LiteralType::Number(6.)),1),
            Token::new(TokenType::RightParen,1),
            Token::new(TokenType::LeftBrace,1),
            Token::new(TokenType::Print,1),
            Token::new(TokenType::Literal(LiteralType::String("hey mom".to_string())),1),
            Token::new(TokenType::Semicolon,1),
            Token::new(TokenType::RightBrace,1),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp.token, token.token);
            } else {
                panic!()
            }
        }
    }

    #[test]
    fn test_unexpected_tokens() {
        let mut lexer = Lexer::new();
        // Fails on first unexpected
        let input = "class main$";

        let result = lexer.scan_tokens(&mut input.chars().peekable());
        let expected = LexError::UnexpectedToken {
            found: '$',
            line: 1,
        };
        assert_eq!(
            result.unwrap_err().downcast::<LexError>().unwrap(),
            expected
        );
    }

    #[test]
    fn test_invalid_number_format() {
        let mut lexer = Lexer::new();
        // Fails because non-digit comes after decimal point
        let input = "1.XXX";

        let result = lexer.scan_tokens(&mut input.chars().peekable());
        let expected = LexError::InvalidNumberFormat;
        assert_eq!(
            result.unwrap_err().downcast::<LexError>().unwrap(),
            expected
        );
    }

    #[test]
    fn test_line_count() {
        let mut lexer = Lexer::new();
        let input = "1
        // random comment2
        3
        4
        5";
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();

        assert_eq!(lexer.line, 5);
    }
}
