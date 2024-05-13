use crate::token::{Token, TokenType};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, pos: 0 }
    }

    fn expect(&mut self, ty: TokenType) {
        if self.tokens[self.pos].ty != ty {
            panic!(
                "unself.expected token: {:?} at {}",
                self.tokens[self.pos], self.pos
            );
        }
        self.pos += 1;
    }

    fn consume(&mut self, ty: TokenType) -> bool {
        let t = &self.tokens[self.pos];
        if t.ty != ty {
            return false;
        } else {
            self.pos += 1;
            true
        }
    }

    fn term(&mut self) -> Node {
        let t = &self.tokens[self.pos];
        self.pos += 1;

        match t.ty {
            TokenType::Num(val) => return Node::new(NodeType::Num(val)),
            TokenType::Ident(ref name) => Node::new(NodeType::Ident(name.clone())),
            TokenType::LeftParen => {
                let node = self.assign();
                self.expect(TokenType::RightParen);
                node
            }
            _ => panic!("number self.expected, but got {}", t.inp),
        }
    }

    fn mul(&mut self) -> Node {
        let mut lhs = self.term();

        loop {
            if self.tokens.len() == self.pos {
                return lhs;
            }

            let op = self.tokens[self.pos].ty.clone();
            if op != TokenType::Mul && op != TokenType::Div {
                return lhs;
            }

            self.pos += 1;
            lhs = Node::new(NodeType::BinOp(op, Box::new(lhs), Box::new(self.term())));
        }
    }

    fn expr(&mut self) -> Node {
        let mut lhs = self.mul();

        loop {
            if self.tokens.len() == self.pos {
                return lhs;
            }

            let op = self.tokens[self.pos].ty.clone();
            if op != TokenType::Plus && op != TokenType::Minus {
                return lhs;
            }

            self.pos += 1;
            let rhs = self.mul();
            lhs = Node::new(NodeType::BinOp(op, Box::new(lhs), Box::new(rhs)));
        }
    }

    fn assign(&mut self) -> Node {
        let lhs = self.expr();

        if self.consume(TokenType::Equal) {
            Node::new(NodeType::BinOp(
                TokenType::Equal,
                Box::new(lhs),
                Box::new(self.expr()),
            ))
        } else {
            lhs
        }
    }

    fn stmt(&mut self) -> Node {
        match self.tokens[self.pos].ty {
            TokenType::If => {
                let mut els = None;

                self.pos += 1;
                self.expect(TokenType::LeftParen);
                let cond = self.assign();
                self.expect(TokenType::RightParen);

                let then = self.stmt();
                if self.consume(TokenType::Else) {
                    els = Some(Box::new(self.stmt()));
                }

                Node::new(NodeType::If(Box::new(cond), Box::new(then), els))
            }
            TokenType::Return => {
                self.pos += 1;
                let expr = self.assign();
                self.expect(TokenType::Semicolon);

                Node::new(NodeType::Return(Box::new(expr)))
            }
            _ => {
                let expr = self.assign();
                let node = Node::new(NodeType::ExprStmt(Box::new(expr)));
                self.expect(TokenType::Semicolon);

                node
            }
        }
    }

    fn compound_stmt(&mut self) -> Node {
        let mut stmts = vec![];

        loop {
            if self.tokens.len() == self.pos {
                let node = Node::new(NodeType::CompStmt(stmts));
                return node;
            }
            stmts.push(self.stmt());
        }
    }

    pub fn parse(&mut self) -> Node {
        self.compound_stmt()
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub ty: NodeType,
}

impl Node {
    pub fn new(ty: NodeType) -> Node {
        Node { ty }
    }
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32),
    Ident(String),
    BinOp(TokenType, Box<Node>, Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Return(Box<Node>),
    ExprStmt(Box<Node>),
    CompStmt(Vec<Node>),
}
