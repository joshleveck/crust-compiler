use lazy_static::lazy_static;
use crate::token::tokentype::TokenType;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: &'static str,
    pub ty: TokenType
}

impl Symbol {
    fn new(name: &'static str, ty: TokenType) -> Self {
        Symbol {
            name,
            ty
        }
    }
}

lazy_static! {
    pub static ref SYMBOLS: Vec<Symbol> = [
        Symbol::new("<<=", TokenType::ShlEQ),
        Symbol::new(">>=", TokenType::ShrEQ),
        Symbol::new("!=", TokenType::NE),
        Symbol::new("&&", TokenType::Logand),
        Symbol::new("++", TokenType::Inc),
        Symbol::new("--", TokenType::Dec),
        Symbol::new("->", TokenType::Arrow),
        Symbol::new("<<", TokenType::SHL),
        Symbol::new("<=", TokenType::LE),
        Symbol::new("==", TokenType::EQ),
        Symbol::new(">=", TokenType::GE),
        Symbol::new(">>", TokenType::SHR),
        Symbol::new("||", TokenType::Logor),
        Symbol::new("*=", TokenType::MulEQ),
        Symbol::new("/=", TokenType::DivEQ),
        Symbol::new("%=", TokenType::ModEQ),
        Symbol::new("+=", TokenType::AddEQ),
        Symbol::new("-=", TokenType::SubEQ),
        Symbol::new("&=", TokenType::BitandEQ),
        Symbol::new("^=", TokenType::XorEQ),
        Symbol::new("|=", TokenType::BitorEQ),
    ]
    .to_vec();
}