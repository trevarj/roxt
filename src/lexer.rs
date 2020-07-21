use super::tokens::{reserved_to_tokentype, LiteralType, Token};
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
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(Token::BangEqual);
                    } else {
                        self.add_token(Token::Bang)
                    }
                }
                '=' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(Token::EqualEqual);
                    } else {
                        self.add_token(Token::Equal);
                    }
                }
                '<' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(Token::LessEqual);
                    } else {
                        self.add_token(Token::Less);
                    }
                }
                '>' => {
                    if source.next_if(|c| *c == '=').is_some() {
                        self.add_token(Token::GreaterEqual);
                    } else {
                        self.add_token(Token::Greater);
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
                        self.add_token(Token::Slash);
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
                    self.add_token(Token::Literal(LiteralType::String(s)));
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
                    self.add_token(Token::Literal(LiteralType::Number(number)));
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
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Dot,
            Token::Minus,
            Token::Plus,
            Token::Star,
            Token::Semicolon,
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(actual) = token_iter.next() {
                assert_eq!(exp, actual);
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
            Token::Bang,
            Token::BangEqual,
            Token::Equal,
            Token::EqualEqual,
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(actual) = token_iter.next() {
                assert_eq!(exp, actual);
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
            Token::Literal(LiteralType::Identifier("Test_Class".to_string())),
            Token::Literal(LiteralType::Identifier("_unused".to_string())),
            Token::Literal(LiteralType::String("my string".to_string())),
            Token::Literal(LiteralType::Number(0.1)),
            Token::Literal(LiteralType::Number(123.)),
            Token::Literal(LiteralType::Number(123.45)),
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp, token);
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
            Token::And,
            Token::Class,
            Token::Else,
            Token::False,
            Token::Fun,
            Token::For,
            Token::If,
            Token::Nil,
            Token::Or,
            Token::Print,
            Token::Return,
            Token::Super,
            Token::This,
            Token::True,
            Token::Var,
            Token::While,
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp, token);
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
            Token::If,
            Token::LeftParen,
            Token::Literal(LiteralType::Identifier("i".to_string())),
            Token::EqualEqual,
            Token::Literal(LiteralType::Number(6.)),
            Token::RightParen,
            Token::LeftBrace,
            Token::Print,
            Token::Literal(LiteralType::String("hey mom".to_string())),
            Token::Semicolon,
            Token::RightBrace,
        ];
        lexer.scan_tokens(&mut input.chars().peekable()).unwrap();
        let mut token_iter = lexer.tokens.iter();
        for exp in expected.iter() {
            if let Some(token) = token_iter.next() {
                assert_eq!(exp, token);
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
