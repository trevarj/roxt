use std::iter::Peekable;
use std::{fmt::Display, str::CharIndices};
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

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "left parenthesis, '('"),
            TokenType::RightParen => write!(f, "right parenthesis, ')'"),
            TokenType::LeftBrace => write!(f, "left brace, '{{'"),
            TokenType::RightBrace => write!(f, "left brace, '}}'"),
            TokenType::Comma => write!(f, "comma, ','"),
            TokenType::Dot => write!(f, "period, '.'"),
            TokenType::Minus => write!(f, "minus, '-'"),
            TokenType::Plus => write!(f, "plus, '+'"),
            TokenType::Semicolon => write!(f, "semicolon, ';'"),
            TokenType::Slash => write!(f, "slash, '/'"),
            TokenType::Star => write!(f, "star, '*'"),
            TokenType::Bang => write!(f, "bang, '!'"),
            TokenType::BangEqual => write!(f, "bang equal, '!='"),
            TokenType::Equal => write!(f, "equal, '='"),
            TokenType::EqualEqual => write!(f, "equal equal, '=='"),
            TokenType::Greater => write!(f, "great than, '>'"),
            TokenType::GreaterEqual => write!(f, "greather than or equal to, '>='"),
            TokenType::Less => write!(f, "less than, '<'"),
            TokenType::LessEqual => write!(f, "less than or equal to, '<='"),
            TokenType::Identifier => write!(f, "identifier"),
            TokenType::String => write!(f, "string constant"),
            TokenType::Number => write!(f, "number constant"),
            TokenType::And => write!(f, "and"),
            TokenType::Class => write!(f, "class"),
            TokenType::Else => write!(f, "else"),
            TokenType::False => write!(f, "false"),
            TokenType::Fun => write!(f, "fun"),
            TokenType::For => write!(f, "for"),
            TokenType::If => write!(f, "if"),
            TokenType::Nil => write!(f, "nil"),
            TokenType::Or => write!(f, "or"),
            TokenType::Print => write!(f, "print"),
            TokenType::Return => write!(f, "return"),
            TokenType::Super => write!(f, "super"),
            TokenType::This => write!(f, "this"),
            TokenType::True => write!(f, "true"),
            TokenType::Var => write!(f, "var"),
            TokenType::While => write!(f, "while"),
            TokenType::EOF => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'input> {
    type_: TokenType,
    lexeme: &'input str,
    line: usize,
}

impl<'input> Token<'_> {
    pub fn new(type_: TokenType, lexeme: &'input str, line: usize) -> Token {
        Token {
            type_,
            lexeme,
            line,
        }
    }

    pub fn ttype(&self) -> TokenType {
        self.type_
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

pub struct Lexer<'input> {
    input: Peekable<CharIndices<'input>>,
    input_str: &'input str,
    line: usize,
}

/// Scans through source code and produces tokens
impl<'input> Lexer<'input> {
    pub fn new(input_str: &'input str) -> Lexer<'input> {
        Lexer {
            input: input_str.char_indices().peekable(),
            input_str,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token<'input>, LexError> {
        // skip white space
        while let Some((_, c)) = self.input.peek() {
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
                        self.input.next();
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
                        self.input.next();
                        self.make_token(TokenType::EqualEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Equal, idx, 1, self.line)
                    }
                }
                '<' => {
                    if let (_, '=') = self.input.peek().ok_or(LexError::UnexpectedEOF {
                        line: self.line,
                        found: c,
                    })? {
                        self.input.next();
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
                        self.input.next();
                        self.make_token(TokenType::GreaterEqual, idx, 2, self.line)
                    } else {
                        self.make_token(TokenType::Greater, idx, 1, self.line)
                    }
                }
                '"' => {
                    let mut length = 0;
                    loop {
                        if let Some((_, next_char)) = self.input.peek().copied() {
                            self.input.next();
                            if let '"' = next_char {
                                // terminated
                                break;
                            }
                            // mult-line string
                            if let '\n' = next_char {
                                self.line += 1;
                            }
                            length += 1;
                        } else {
                            return Err(LexError::UnterminatedString);
                        }
                    }
                    self.make_token(TokenType::String, idx + 1, length, self.line)
                }
                // Numbers
                '0'..='9' => {
                    let mut length = 1;
                    while let Some((_, '0'..='9')) = self.input.peek() {
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

                c => {
                    return Err(LexError::UnexpectedToken {
                        found: c,
                        line: self.line,
                    })
                }
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

    fn make_ident_token(&self, start_idx: usize, length: usize, line: usize) -> Token<'input> {
        let lexeme = &self.input_str[start_idx..start_idx + length];
        // check for keyword
        let type_ = reserved_to_tokentype(lexeme);
        Token::new(type_, lexeme, line)
    }

    fn make_token(
        &self,
        type_: TokenType,
        start_idx: usize,
        length: usize,
        line: usize,
    ) -> Token<'input> {
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
        let input_str = r#" "asdasd" 34.2 <= 444 // asdasdasd fuck
        ((   !=) == class fuck "#;
        // let input_str = "5 <= 6";
        // let input_str = r#""hi" == "hi""#;
        let mut lexer = Lexer::new(input_str);

        loop {
            let t = lexer.scan_token();
            // assert!(t.is_ok());
            let t = t.unwrap();
            println!("{:?}", t);
            if let TokenType::EOF = t.type_ {
                break;
            }
        }
    }
}
