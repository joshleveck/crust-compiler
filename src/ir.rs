use crate::parse::{Node, NodeType};
use crate::token::TokenType;

use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;
use std::sync::Mutex;

lazy_static! {
    static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
    static ref REGNO: Mutex<usize> = Mutex::new(1);
    static ref BASE_REG: Mutex<usize> = Mutex::new(0);
    static ref BPOFF: Mutex<usize> = Mutex::new(0);
    static ref LABEL: Mutex<usize> = Mutex::new(0);
    static ref IRINFO: [IRInfo; 16] = [
        IRInfo::new(IROp::Add, "+", IRType::RegReg),
        IRInfo::new(IROp::Sub, "-", IRType::RegReg),
        IRInfo::new(IROp::Mul, "*", IRType::RegReg),
        IRInfo::new(IROp::Div, "/", IRType::RegReg),
        IRInfo::new(IROp::Imm, "MOV", IRType::RegImm),
        IRInfo::new(IROp::AddImm, "ADD", IRType::RegImm),
        IRInfo::new(IROp::Mov, "MOV", IRType::RegReg),
        IRInfo::new(IROp::Label, "", IRType::Label),
        IRInfo::new(IROp::Jmp, "", IRType::Label),
        IRInfo::new(IROp::Unless, "UNLESS", IRType::RegLabel),
        IRInfo::new(IROp::Return, "RET", IRType::Reg),
        IRInfo::new(IROp::Alloca, "ALLOCA", IRType::RegImm),
        IRInfo::new(IROp::Load, "LOAD", IRType::RegReg),
        IRInfo::new(IROp::Store, "STORE", IRType::RegReg),
        IRInfo::new(IROp::Kill, "KILL", IRType::Reg),
        IRInfo::new(IROp::Nop, "NOP", IRType::Noarg),
    ];
}

#[derive(Clone)]
pub enum IRType {
    Noarg,
    Reg,
    RegImm,
    RegReg,
    RegLabel,
    Label,
}

#[derive(Clone)]
pub struct IRInfo {
    pub op: IROp,
    pub name: &'static str,
    pub ty: IRType,
}

impl IRInfo {
    pub fn new(op: IROp, name: &'static str, ty: IRType) -> IRInfo {
        IRInfo {
            op: op,
            name: name,
            ty: ty,
        }
    }
}

impl fmt::Display for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IRType::*;

        let info = get_irinfo(self);
        let lhs = self.lhs.unwrap();

        match info.ty {
            Noarg => write!(f, "{}\n", info.name),
            Reg => write!(f, "{} r{}\n", info.name, lhs),
            RegImm => write!(f, "{} r{} {}\n", info.name, lhs, self.rhs.unwrap()),
            RegReg => write!(f, "{} r{} r{}\n", info.name, lhs, self.rhs.unwrap()),
            RegLabel => write!(f, "{} r{} .L{}\n", info.name, lhs, self.rhs.unwrap()),
            Label => write!(f, ".L{}=>\n", lhs),
        }
    }
}

pub fn dump_ir(irv: &Vec<Ir>) {
    for ir in irv {
        println!("{}", ir);
    }
}

pub fn get_irinfo(ir: &Ir) -> IRInfo {
    for info in IRINFO.iter() {
        if info.op == ir.op {
            return info.clone();
        }
    }

    panic!("Unknown IR {:?}", ir);
}

#[derive(Debug, Clone, PartialEq)]
pub enum IROp {
    Nop,
    Mov,
    Imm,
    Add,
    AddImm,
    Sub,
    Mul,
    Div,
    Return,
    Label,
    Jmp,
    Unless,
    Alloca,
    Load,
    Store,
    Kill,
}

impl From<NodeType> for IROp {
    fn from(node_type: NodeType) -> Self {
        match node_type {
            NodeType::BinOp(op, _, _) => Self::from(op),
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

impl From<TokenType> for IROp {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => IROp::Add,
            TokenType::Minus => IROp::Sub,
            TokenType::Mul => IROp::Mul,
            TokenType::Div => IROp::Div,
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub op: IROp,
    pub lhs: Option<usize>,
    pub rhs: Option<usize>,
}

impl Ir {
    pub fn new(op: IROp, lhs: Option<usize>, rhs: Option<usize>) -> Ir {
        Ir {
            op: op,
            lhs: lhs,
            rhs: rhs,
        }
    }
}

fn gen_lval(code: &mut Vec<Ir>, node: Node) -> Option<usize> {
    match node.ty {
        NodeType::Ident(name) => {
            if VARS.lock().unwrap().get(&name).is_none() {
                VARS.lock()
                    .unwrap()
                    .insert(name.clone(), *BPOFF.lock().unwrap());
                *BPOFF.lock().unwrap() += 8;
            }

            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;

            let off = *VARS.lock().unwrap().get(&name).unwrap();
            code.push(Ir::new(IROp::Mov, r, Some(*BASE_REG.lock().unwrap())));
            code.push(Ir::new(IROp::AddImm, r, Some(off)));

            r
        }
        _ => panic!("not an lvalue"),
    }
}

fn gen_expr(code: &mut Vec<Ir>, node: Node) -> Option<usize> {
    match node.ty {
        NodeType::Num(val) => {
            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;
            code.push(Ir::new(IROp::Imm, r, Some(val as usize)));

            r
        }
        NodeType::Ident(_) => {
            let r = gen_lval(code, node);
            code.push(Ir::new(IROp::Load, r, r));

            r
        }
        NodeType::BinOp(op, lhs, rhs) => match op {
            TokenType::Equal => {
                let rhs = gen_expr(code, *rhs);
                let lhs = gen_lval(code, *lhs);
                code.push(Ir::new(IROp::Store, lhs, rhs));
                code.push(Ir::new(IROp::Kill, rhs, None));

                lhs
            }
            _ => {
                let lhs = gen_expr(code, *lhs);
                let rhs = gen_expr(code, *rhs);

                code.push(Ir::new(IROp::from(op), lhs, rhs));
                code.push(Ir::new(IROp::Kill, rhs, None));

                lhs
            }
        },
        _ => unreachable!(),
    }
}

fn gen_stmt(code: &mut Vec<Ir>, node: Node) {
    match node.ty {
        NodeType::If(cond, then, els_may) => {
            let r = gen_expr(code, *cond);
            let x = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;
            code.push(Ir::new(IROp::Unless, r, x));
            code.push(Ir::new(IROp::Kill, r, None));
            gen_stmt(code, *then);

            if let Some(els) = els_may {
                let y = Some(*LABEL.lock().unwrap());
                *LABEL.lock().unwrap() += 1;
                code.push(Ir::new(IROp::Jmp, y, None));
                code.push(Ir::new(IROp::Label, x, None));
                gen_stmt(code, *els);
                code.push(Ir::new(IROp::Label, y, None));
                return;
            } else {
                code.push(Ir::new(IROp::Label, x, None));
                return;
            }
        }
        NodeType::Return(expr) => {
            let r = gen_expr(code, *expr);
            code.push(Ir::new(IROp::Return, r, None));
            code.push(Ir::new(IROp::Kill, r, None));
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(code, *expr);
            code.push(Ir::new(IROp::Kill, r, None));
        }
        NodeType::CompStmt(stmts) => {
            for n in stmts {
                gen_stmt(code, n);
            }
        }
        e => panic!("unknown node: {:?}", e),
    }
}

pub fn gen_ir(node: Node) -> Vec<Ir> {
    let mut code = vec![];

    code.push(Ir::new(IROp::Alloca, Some(*BASE_REG.lock().unwrap()), None));
    gen_stmt(&mut code, node);
    code[0].rhs = Some(*BPOFF.lock().unwrap());
    code.push(Ir::new(IROp::Kill, Some(*BASE_REG.lock().unwrap()), None));
    code
}
