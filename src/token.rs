#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Num(i32),
    Ident(String),
    Semicolon,
    Plus,
    Minus,
    Mul,
    Div,
    LeftParen,
    RightParen,
    If,
    Else,
    Equal,
    Return,
}

impl From<char> for TokenType {
    fn from(c: char) -> Self {
        match c {
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Mul,
            '/' => TokenType::Div,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            ';' => TokenType::Semicolon,
            '=' => TokenType::Equal,
            _ => panic!("cannot convert: {:?}", c),
        }
    }
}

impl From<String> for TokenType {
    fn from(s: String) -> Self {
        match s.as_str() {
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            _ => panic!("cannot convert: {:?}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub inp: String,
}

pub fn scan(mut inp: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let org = inp.clone();

    while let Some(c) = inp.chars().nth(0) {
        if c.is_whitespace() {
            inp = inp.split_off(1);
            continue;
        }

        match c {
            '+' | '-' | '*' | '/' | '(' | ')' | ';' | '=' => {
                tokens.push(Token {
                    ty: TokenType::from(c),
                    inp: org.clone(),
                });
                inp = inp.split_off(1);
            }
            _ => {
                if c.is_alphabetic() || c == '_' {
                    let mut s = String::new();
                    while let Some(c1) = inp.chars().nth(0) {
                        if c1.is_alphanumeric() || c1 == '_' || c1.is_ascii_digit() {
                            s.push(c1);
                            inp = inp.split_off(1);
                        } else {
                            break;
                        }
                    }

                    tokens.push(Token {
                        ty: TokenType::from(s),
                        inp: org.clone(),
                    });
                } else if c.is_ascii_digit() {
                    let n = strtol(&mut inp);
                    tokens.push(Token {
                        ty: TokenType::Num(n.unwrap() as i32),
                        inp: org.clone(),
                    });
                } else {
                    panic!("cannot convert: {:?}", c);
                }
            }
        }
    }

    tokens
}

pub fn tokenize(p: String) -> Vec<Token> {
    scan(p)
}

fn strtol(s: &mut String) -> Option<i64> {
    if s.is_empty() {
        return None;
    }

    let mut pos = 0;
    for c in s.chars() {
        if !c.is_ascii_digit() {
            break;
        }
        pos += 1;
    }

    let t: String = s.drain(..pos).collect();
    Some(t.parse::<i64>().unwrap())
}
