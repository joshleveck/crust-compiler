use core::fmt;
use std::collections::HashMap;

pub mod symbol;
pub mod tokentype;
pub mod ctypes;

use crate::preprocess;
use symbol::SYMBOLS;
use tokentype::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum CharType {
    WhiteSpace,
    NewLine,
    Alpha,
    Digit,
    NonAlpha(char),
    Unknown(char),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,

    pub stringize: bool,

    pub buf: String,
    pub filename: String,
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.ty)
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            ty: TokenType::NewLine,
            stringize: false,
            buf: String::new(),
            filename: String::new(),
            start: 0,
            end: 0,
            line: 0,
        }
    }
}

impl Token {
    pub fn new(
        ty: TokenType,
        start: usize,
        end: usize,
        line: usize,
        buf: String,
        filename: String,
    ) -> Self {
        Token {
            ty,
            stringize: false,
            buf,
            filename,
            start,
            end,
            line,
        }
    }

    pub fn bad_token(&self, msg: &str) -> ! {
        panic!("{}:{}:{}: {}", self.filename, self.start, self.end, msg);
    }

    pub fn tokstr(&self) -> String {
        self.buf[self.start..self.end].to_string()
    }

    pub fn get_line_number(&self) -> usize {
        self.buf[..self.end].chars().collect::<Vec<char>>().iter().filter(|c| *c == &'\n').count()
    }

    pub fn is_ident(&self, s: &str) -> bool {
        match self.ty {
            TokenType::Ident(ref x) => x == s, 
            _ => false,
        }
    }
}

struct Tokenizer {
    p: Vec<char>,
    data: String,
    pos: usize,
    tokens: Vec<Token>,

    filename: String,
}

impl Tokenizer {
    fn new(filename: String, data: String) -> Self {
        Tokenizer {
            p: data.chars().collect(),
            data,
            pos: 0,
            tokens: Vec::new(),
            filename,
        }
    }

    fn eat_token(&mut self, ty: TokenType, size: usize) {
        let mut t = self.new_token(ty);
        t.end = self.pos + size;
        self.pos += size;
        self.tokens.push(t);
    }

    fn new_token(&self, ty: TokenType) -> Token {
        Token::new(
            ty,
            self.pos,
            self.pos,
            0,
            self.data.clone(),
            self.filename.clone(),
        )
    }

    fn get_character(&self, advance_from_pos: usize) -> Option<CharType> {
        self.p.get(self.pos + advance_from_pos).map(|c| {
            if c.is_whitespace() {
                CharType::WhiteSpace
            } else if c == &'\n' {
                CharType::NewLine
            } else if c.is_alphabetic() || c == &'_' {
                CharType::Alpha
            } else if c.is_ascii_digit() {
                CharType::Digit
            } else {
                CharType::NonAlpha(*c)
            } 
        })
    }

    fn scan(&mut self, keywords: &HashMap<String, TokenType>) -> Vec<Token> {
        'outer: while let Some(head_char) = self.get_character(0) {
            match head_char {
                CharType::NewLine => self.eat_token(TokenType::NewLine, 1),
                CharType::WhiteSpace => self.pos += 1,
                CharType::Alpha => self.ident(&keywords),
                CharType::Digit => self.number(),

                CharType::NonAlpha('\'') => self.char_literal(),
                CharType::NonAlpha('"') => self.string_literal(),
                CharType::NonAlpha('/') => match self.p.get(self.pos + 1) {
                    Some('/') => self.line_comment(),
                    Some('*') => self.block_comment(),
                    Some('=') => self.eat_token(TokenType::DivEQ, 2),
                    _ => self.eat_token(TokenType::Div, 1),
                },
                CharType::NonAlpha(c) => {
                    for symbol in SYMBOLS.iter() {
                        let name = symbol.name;
                        let len = name.len();
                        if self.pos + len > self.p.len() {
                            continue;
                        }

                        let first = self.p[self.pos..self.pos + len].iter().collect::<String>();
                        if name != first {
                            continue;
                        }

                        self.eat_token(symbol.ty.clone(), len);
                        continue 'outer;
                    }

                    if let Some(ty) = TokenType::new_single_letter(c) {
                        self.eat_token(ty, 1);
                        continue 'outer;
                    }

                    self.bad_position("Unknown");
                }
                CharType::Unknown(c) => self.bad_position(&format!("Unknown: {}", c)),
            }
        }

        self.tokens.clone()
    }

    fn line_comment(&mut self) {
        while self.p.get(self.pos) != Some(&'\n') {
            self.pos += 1;
        }
    }

    fn block_comment(&mut self) {
        self.pos += 2;
        loop {
            if let Some(two_char) = self.p.get(self.pos..self.pos + 2) {
                self.pos += 1;
                if two_char == ['*', '/'] {
                    self.pos += 1;
                    return;
                }
            } else {
                self.bad_position("unclosed comment");
            }
        }
    }

    fn escaped(c: char) -> Option<char> {
        match c {
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            _ => None,
        }
    }

    fn char_literal(&mut self) {
        let mut end = self.pos;
        end += 1;
        let c = self.p.get(end).expect("unclosed char literal");

        let result = if c != &'\\' {
            *c
        } else {
            end += 1;
            let c = self.p.get(end).expect("unclosed char literal");
            if let Some(c) = Self::escaped(*c) {
                c
            } else {
                *c
            }
        };

        end += 1;

        if self.p.get(end) != Some(&'\'') {
            self.bad_position("unclosed char literal");
        }

        self.eat_token(TokenType::Num(result as u8 as i32), end - self.pos + 1);
    }

    fn string_literal(&mut self) {
        let mut end = self.pos + 1;
        let mut buf = String::new();

        while let Some(c) = self.p.get(end) {
            if c == &'\\' {
                end += 1;
                let c = self.p.get(end).expect("unclosed string literal");
                if let Some(c) = Self::escaped(*c) {
                    buf.push(c);
                } else {
                    buf.push(*c);
                }
            } else if c == &'"' {
                break;
            } else {
                buf.push(*c);
            }
            end += 1;
        }

        if self.p.get(end) != Some(&'"') {
            self.bad_position("unclosed string literal");
        }

        self.eat_token(TokenType::Str(buf, end - self.pos + 1), end - self.pos + 1);
    }

    fn ident(&mut self, keywords: &HashMap<String, TokenType>) {
        let mut len = 1;
        while let Some(c2) = self.p.get(self.pos + len) {
            if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == &'_' {
                len += 1;
            } else {
                break;
            }
        }

        let name = self.p[self.pos..self.pos + len].iter().collect::<String>();

        let ty = if let Some(t) = keywords.get(&name) {
            t.clone()
        } else {
            TokenType::Ident(name)
        };
    
        self.eat_token(ty, len);
    }

    fn number(&mut self) {
        match self.p.get(self.pos..self.pos + 2) {
            Some(['0', 'x']) | Some(['0', 'X']) => self.parse_number(16),
            Some(['0', 'b']) | Some(['0', 'B']) => self.parse_number(2),
            Some(['0', 'o']) | Some(['0', 'O']) => self.parse_number(8),
            _ => self.parse_number(10),
        }
    }

    fn parse_number(&mut self, base: u32) {
        let mut end = self.pos;
        while let Some(CharType::Digit) = self.get_character(end - self.pos) {
            end += 1;
        }

        let buf = self.p[self.pos..end].iter().collect::<String>();
        let num = i32::from_str_radix(&buf, base).expect("invalid number");
        self.eat_token(TokenType::Num(num), end - self.pos);
    }

    fn canonicalize_newline(&mut self) {
        let mut pos = 0;
        while pos < self.p.len() {
            if self.p[pos] == '\r' && self.p.get(pos + 1) == Some(&'\n') {
                self.p[pos] = '\n';
                self.p.remove(pos + 1);
            }
            pos += 1;
        }
    }

    fn remove_backslash_newline(&mut self) {
        let mut pos = 0;
        while pos < self.p.len() {
            if self.p[pos] == '\\' && self.p.get(pos + 1) == Some(&'\n') {
                self.p.remove(pos);
                self.p.remove(pos);
            }
            pos += 1;
        }
    }

    fn append(&mut self, x_str: &str, y_str: &str, start: usize) -> Token {
        let concatendated = format!("{}{}", x_str, y_str);
        let len = concatendated.len();
        Token::new(
            TokenType::Str(concatendated.clone(), len),
            start,
            start + len,
            0,
            concatendated,
            self.filename.clone(),
        )
    }

    fn join_string_literals(&mut self) {
        let mut v = vec![];
        let mut last: Option<Token> = None;

        for t in self.tokens.clone().into_iter() {
            if let Some(ref last) = last {
                if let (TokenType::Str(x, _), TokenType::Str(y, _)) = (&last.ty, &t.ty) {
                    let new_token = self.append(&x, &y, last.start);
                    v.pop();
                    v.push(new_token);
                    continue;
                }

            }
            last = Some(t.clone());
            v.push(t);
        }

        self.tokens = v;
    }

    fn strip_newlines_tokens(&mut self) {
        self.tokens = self
            .tokens
            .clone()
            .into_iter()
            .filter(|t| t.ty != TokenType::NewLine)
            .collect();
    }

    fn bad_position(&self, msg: &str) -> ! {
        panic!("{}:{}: {}", self.filename, self.pos, msg);
    }
}

fn keyword_map() -> HashMap<String, TokenType> {
    let mut map = HashMap::new();
    map.insert("_Alignof".into(), TokenType::Alignof);
    map.insert("break".into(), TokenType::Break);
    map.insert("char".into(), TokenType::Char);
    map.insert("void".into(), TokenType::Void);
    map.insert("do".into(), TokenType::Do);
    map.insert("else".into(), TokenType::Else);
    map.insert("extern".into(), TokenType::Extern);
    map.insert("for".into(), TokenType::For);
    map.insert("if".into(), TokenType::If);
    map.insert("int".into(), TokenType::Int);
    map.insert("return".into(), TokenType::Return);
    map.insert("sizeof".into(), TokenType::Sizeof);
    map.insert("struct".into(), TokenType::Struct);
    map.insert("typedef".into(), TokenType::Typedef);
    map.insert("while".into(), TokenType::While);
    map
}

pub fn tokenize(filename: String, data: String,  ctx: &mut preprocess::Preprocessor) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(filename, data);
    tokenizer.canonicalize_newline();
    tokenizer.remove_backslash_newline();
    tokenizer.scan(&keyword_map());

    tokenizer.tokens = preprocess::preprocess(tokenizer.tokens, ctx);
    tokenizer.strip_newlines_tokens();
    tokenizer.join_string_literals();
    tokenizer.tokens
}
