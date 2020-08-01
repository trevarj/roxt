use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum LexError {
    #[error("Unexpected token {found:?} found on line {line:?}.")]
    UnexpectedToken { found: char, line: usize },
    #[error("Unterminated string.")]
    UnterminatedString,
    #[error("Invalid number format. No digit provided after decimal point?")]
    InvalidNumberFormat,
    #[error("Unexpected EOF on {line:?}, found {found:?}.")]
    UnexpectedEOF { line: usize, found: char },
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End of file
    EOF,
}

#[derive(Debug)]
pub struct Token<'l> {
    type_: TokenType,
    lexeme: &'l str,
    line: usize,
}

impl<'l> Token<'_> {
    pub fn new(type_: TokenType, lexeme: &'l str, line: usize) -> Token<'l> {
        Token {
            type_,
            lexeme,
            line,
        }
    }
}

pub struct Lexer<'input> {
    input: &'input mut Peekable<CharIndices<'input>>,
    input_str: &'input str,
    line: usize,
}

/// Scans through source code and produces tokens
impl Lexer<'_> {
    pub fn new<'input>(
        input_str: &'input str,
        input: &'input mut Peekable<CharIndices<'input>>,
    ) -> Lexer<'input> {
        Lexer {
            input,
            input_str,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, LexError> {
        // skip white space
        while let Some((idx, c)) = self.input.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.input.next();
                }
                '\n' => {
                    self.line += 1;
                    self.input.next();
                }
                '/' => {
                    if let Some((_, '/')) = self.peek_next() {
                        while let Some((_, c)) = self.input.peek() {
                            if let '\n' = c {
                                break;
                            }
                            self.input.next();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Ok(if let Some((idx, c)) = self.input.next() {
            match c {
                // Single char tokens
                '(' => self.make_token(TokenType::LeftParen, idx, 1, self.line),
                ')' => self.make_token(TokenType::RightParen, idx, 1, self.line),
                '{' => self.make_token(TokenType::LeftBrace, idx, 1, self.line),
                '}' => self.make_token(TokenType::RightBrace, idx, 1, self.line),
                ',' => self.make_token(TokenType::Comma, idx, 1, self.line),
                '.' => self.make_token(TokenType::Dot, idx, 1, self.line),
                '-' => self.make_token(TokenType::Minus, idx, 1, self.line),
                '+' => self.make_token(TokenType::Plus, idx, 1, self.line),
                '*' => self.make_token(TokenType::Star, idx, 1, self.line),
                '/' => self.make_token(TokenType::Slash, idx, 1, self.line),
                ';' => self.make_token(TokenType::Semicolon, idx, 1, self.line),
                '!' => {
                    if let (_, '=') = self.input.peek().ok_or(LexError::UnexpectedEOF {
                        line: self.line,
                        found: c,
                    })? {
                        self.make_token(TokenType::BangEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Bang, idx, 1, self.line)
                    }
                }
                '=' => {
                    if let (_, '=') = self.input.peek().ok_or(LexError::UnexpectedEOF {
                        line: self.line,
                        found: c,
                    })? {
                        self.make_token(TokenType::BangEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Equal, idx, 1, self.line)
                    }
                }
                '<' => {
                    if let (_, '=') = self.input.peek().ok_or(LexError::UnexpectedEOF {
                        line: self.line,
                        found: c,
                    })? {
                        self.make_token(TokenType::LessEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Less, idx, 1, self.line)
                    }
                }
                '>' => {
                    if let (_, '=') = self.input.peek().ok_or(LexError::UnexpectedEOF {
                        line: self.line,
                        found: c,
                    })? {
                        self.make_token(TokenType::GreaterEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Greater, idx, 1, self.line)
                    }
                }
                '"' => {
                    let mut length = 0;
                    while let Some((_, next_char)) = self.input.peek().copied() {
                        if let '\n' = next_char {
                            self.line += 1;
                        }
                        if let '"' = next_char {
                            // eat the closing quote
                            self.input.next();
                            break;
                        }
                        length += 1;
                        self.input.next();
                    }
                    if self.input.peek().is_none() {
                        return Err(LexError::UnterminatedString);
                    }
                    self.make_token(TokenType::String, idx + 1, length, self.line)
                }
                // Numbers
                '0'..='9' => {
                    let mut length = 1;
                    while let Some((idx, '0'..='9')) = self.input.peek() {
                        self.input.next();
                        length += 1;
                    }
                    // check for fractional number
                    if let (Some((_, '.')), Some((_, '0'..='9'))) =
                        (self.input.peek().copied(), self.peek_next())
                    {
                        // consume dot and scan digits
                        length += 1;
                        self.input.next();
                        while let Some((_, '0'..='9')) = self.input.peek() {
                            self.input.next();
                            length += 1;
                        }
                    }
                    self.make_token(TokenType::Number, idx, length, self.line)
                }
                // Identifiers and Keywords
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut length = 1;
                    while let Some((_, c)) = self.input.peek() {
                        if c.is_ascii_alphanumeric() || *c == '_' {
                            self.input.next();
                            length += 1;
                        } else {
                            break;
                        }
                    }
                    self.make_ident_token(idx, length, self.line)
                }

                c => todo!(),
            }
        } else {
            Token::new(TokenType::EOF, "EOF", self.line)
        })
    }

    fn peek_next(&self) -> Option<(usize, char)> {
        let mut peeker = self.input.clone();
        peeker.next();
        peeker.peek().copied()
    }

    fn make_ident_token(&self, start_idx: usize, length: usize, line: usize) -> Token {
        let lexeme = &self.input_str[start_idx..start_idx + length];
        // check for keyword
        let type_ = reserved_to_tokentype(lexeme);
        Token::new(type_, lexeme, line)
    }

    fn make_token(&self, type_: TokenType, start_idx: usize, length: usize, line: usize) -> Token {
        let lexeme = &self.input_str[start_idx..start_idx + length];
        Token::new(type_, lexeme, line)
    }
}

fn reserved_to_tokentype(keyword: &str) -> TokenType {
    match keyword {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "fun" => TokenType::Fun,
        "for" => TokenType::For,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        _ => TokenType::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input_str = r#" "asdasd" 34.2  444 // asdasdasd fuck
        ((   !=)  class fuck "#;
        let mut peekable = input_str.char_indices().peekable();
        let mut lexer = Lexer::new(input_str, &mut peekable);

        loop {
            let t = lexer.scan_token();
            assert!(t.is_ok());
            let t = t.unwrap();
            println!("{:?}", t);
            if let TokenType::EOF = t.type_ {
                break;
            }
        }
    }
}
