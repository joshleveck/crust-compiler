use crate::token::ctypes::{Ctype, Node, NodeType, Scope, Type, Var};
use crate::util::roundup;
use env::Env;
use std::sync::Mutex;

use lazy_static::lazy_static;
mod env;

fn swap(p: &mut Node, q: &mut Node) {
    std::mem::swap(p, q);
}

lazy_static! {
    static ref GLOBALS: Mutex<Vec<Var>> = Mutex::new(vec![]);
    static ref ENV: Mutex<Env> = Mutex::new(Env::new(None));
    static ref STRLABEL: Mutex<usize> = Mutex::new(0);
    static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
}

fn into_new_range<T: Sized>(param: T, f: Box<dyn Fn(T) -> T>) -> T {
    let env = ENV.lock().unwrap().clone();
    *ENV.lock().unwrap() = Env::new(Some(Box::new(env)));
    let ret = f(param);

    let env = ENV.lock().unwrap().clone();
    *ENV.lock().unwrap() = *env.next.unwrap();
    ret
}

fn find_var(name: &str) -> Option<Var> {
    let env = ENV.lock().unwrap().clone();
    let mut next = &Some(Box::new(env));

    loop {
        if let Some(ref e) = next {
            if let Some(var) = e.vars.get(name) {
                return Some(var.clone());
            } else {
                next = &e.next;
            }
        } else {
            return None;
        }
    }
}

fn maybe_decay(base: Node, decay: bool) -> Node {
    if !decay {
        return base;
    }

    if let Ctype::Array(ary_of, _) = base.ty.ty.clone() {
        let mut node = Node::new(NodeType::Addr(Box::new(base)));
        node.ty = Box::new(Type::ptr_to(ary_of.clone()));
        node
    } else {
        base
    }
}

fn check_lval(node: &Node) {
    match node.op {
        NodeType::Lvar(_)
        | NodeType::Gvar(_, _, _)
        | NodeType::Deref(_)
        | NodeType::Dot(_, _, _) => {}
        _ => panic!("not an lvalue"),
    }
}

fn walk(mut node: Node, decay: bool) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) | Null | Break => (),
        Str(data, len) => {
            let name = format!(".L.str{}", *STRLABEL.lock().unwrap());
            *STRLABEL.lock().unwrap() += 1;
            let var = Var::new_global(node.ty.clone(), name, data, len, false);
            let name = var.name.clone();
            GLOBALS.lock().unwrap().push(var);

            let mut ret = Node::new(NodeType::Gvar(name, "".into(), len));
            ret.ty = node.ty;
            return maybe_decay(ret, decay);
        }
        Ident(ref name) => {
            if let Some(var) = find_var(name) {
                match var.scope {
                    Scope::Local(offset) => {
                        let mut ret = Node::new(NodeType::Lvar(Scope::Local(offset)));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                    Scope::Global(ref data, len, _) => {
                        let mut ret =
                            Node::new(NodeType::Gvar(var.name.clone(), data.clone(), len));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                }
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may, _) => {
            let stacksize = *STACKSIZE.lock().unwrap();
            *STACKSIZE.lock().unwrap() = roundup(stacksize, node.ty.align);
            *STACKSIZE.lock().unwrap() += node.ty.size;
            let offset = *STACKSIZE.lock().unwrap();

            ENV.lock().unwrap().vars.insert(
                name.clone(),
                Var::new(node.ty.clone(), name.clone(), Scope::Local(offset)),
            );

            let mut init = None;
            if let Some(init2) = init_may {
                init = Some(Box::new(walk(*init2, true)));
            }
            node.op = Vardef(name, init, Scope::Local(offset));
        }
        If(mut cond, mut then, els_may) => {
            cond = Box::new(walk(*cond, true));
            then = Box::new(walk(*then, true));
            let mut new_els = None;
            if let Some(els) = els_may {
                new_els = Some(Box::new(walk(*els, true)));
            }
            node.op = If(cond, then, new_els);
        }
        Ternary(mut cond, mut then, mut els) => {
            cond = Box::new(walk(*cond, true));
            then = Box::new(walk(*then, true));
            els = Box::new(walk(*els, true));
            node.ty = then.ty.clone();
            node.op = Ternary(cond, then, els);
        }
        For(init, cond, inc, body) => {
            let f = |(init, cond, inc, body)| -> (Node, Node, Node, Node) {
                (
                    walk(init, true),
                    walk(cond, true),
                    walk(inc, true),
                    walk(body, true),
                )
            };
            let (init, cond, inc, body) = into_new_range((*init, *cond, *inc, *body), Box::new(f));
            node.op = For(
                Box::new(init),
                Box::new(cond),
                Box::new(inc),
                Box::new(body),
            );
        }
        DoWhile(body, cond) => {
            node.op = DoWhile(Box::new(walk(*body, true)), Box::new(walk(*cond, true)));
        }
        Dot(mut expr, name, _) => {
            expr = Box::new(walk(*expr, true));
            let offset;
            if let Ctype::Struct(ref members) = expr.ty.ty {
                if members.is_empty() {
                    panic!("incomplete type");
                }
                let m_may = members.iter().find(|m| {
                    if let NodeType::Vardef(ref m_name, _, _) = m.op {
                        if m_name != &name {
                            return false;
                        }
                        return true;
                    }
                    false
                });

                if let Some(m) = m_may {
                    if let NodeType::Vardef(_, _, Scope::Local(offset2)) = m.op {
                        node.ty = m.ty.clone();
                        offset = offset2;
                    } else {
                        unreachable!()
                    }
                } else {
                    panic!("member missing: {}", name);
                }
            } else {
                panic!("struct expected before '.'");
            }

            node.op = NodeType::Dot(expr, name, offset);
            return maybe_decay(node, decay);
        }
        BinOp(token_type, mut lhs, mut rhs) => {
            use crate::token::tokentype::TokenType::*;
            match token_type {
                Plus | Minus => {
                    lhs = Box::new(walk(*lhs, true));
                    rhs = Box::new(walk(*rhs, true));

                    if matches!(rhs.ty.ty, Ctype::Ptr(_)) {
                        swap(&mut lhs, &mut rhs);
                    }
                    if matches!(rhs.ty.ty, Ctype::Ptr(_)) {
                        panic!("'pointer {:?} pointer' is not defined", node.op.clone())
                    }

                    if matches!(lhs.ty.ty, Ctype::Ptr(_)) {
                        rhs = Box::new(Node::scale_ptr(rhs, &lhs.ty));
                    }

                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
                AddEQ | SubEQ => {
                    lhs = Box::new(walk(*lhs, false));
                    check_lval(&*lhs);
                    rhs = Box::new(walk(*rhs, true));

                    if matches!(lhs.ty.ty, Ctype::Ptr(_)) {
                        rhs = Box::new(Node::scale_ptr(rhs, &lhs.ty));
                    }
                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
                Equal | MulEQ | DivEQ | ModEQ | ShlEQ | ShrEQ | BitandEQ | XorEQ | BitorEQ => {
                    lhs = Box::new(walk(*lhs, false));
                    check_lval(&*lhs);
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs, true)));
                    node.ty = lhs.ty;
                }
                _ => {
                    lhs = Box::new(walk(*lhs, true));
                    rhs = Box::new(walk(*rhs, true));
                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
            }
        }
        PostInc(mut expr) => {
            expr = Box::new(walk(*expr, true));
            node.ty = expr.ty.clone();
            node.op = PostInc(expr);
        }
        PostDec(mut expr) => {
            expr = Box::new(walk(*expr, true));
            node.ty = expr.ty.clone();
            node.op = PostDec(expr);
        }
        Neg(mut expr) => {
            expr = Box::new(walk(*expr, true));
            node.ty = expr.ty.clone();
            node.op = Neg(expr);
        }
        Exclamation(mut expr) => {
            expr = Box::new(walk(*expr, true));
            node.ty = expr.ty.clone();
            node.op = Exclamation(expr);
        }
        Addr(mut expr) => {
            expr = Box::new(walk(*expr, true));
            check_lval(&*expr);
            node.ty = Box::new(Type::ptr_to(expr.ty.clone()));
            node.op = Addr(expr);
        }
        Deref(mut expr) => {
            expr = Box::new(walk(*expr, true));
            match expr.ty.ty {
                Ctype::Ptr(ref ptr_to) => node.ty = ptr_to.clone(),
                Ctype::Void => panic!("cannot dereference void pointer"),
                _ => panic!("operand must be a pointer"),
            }
            node.op = Deref(expr);
            return maybe_decay(node, decay);
        }
        Return(expr) => node.op = Return(Box::new(walk(*expr, true))),
        ExprStmt(expr) => node.op = ExprStmt(Box::new(walk(*expr, true))),
        Sizeof(mut expr) => {
            expr = Box::new(walk(*expr, false));
            node = Node::new_int(expr.ty.size as i32)
        }
        Alignof(mut expr) => {
            expr = Box::new(walk(*expr, false));
            node = Node::new_int(expr.ty.align as i32)
        }
        Call(name, mut args) => {
            if let Some(var) = find_var(&name) {
                if let Ctype::Func(returning) = var.ty.ty {
                    node.ty = returning;
                } else {
                    eprint!("bad function: {}", name);
                }
            } else {
                eprint!("bad function: {}", name);
            }

            args = args.into_iter().map(|arg| walk(arg, true)).collect();
            node.op = Call(name, args);
        }
        CompStmt(mut stmts) => {
            let f = |stmts: Vec<Node>| -> Vec<Node> {
                stmts.into_iter().map(|stmt| walk(stmt, true)).collect()
            };
            stmts = into_new_range(stmts, Box::new(f));
            node.op = CompStmt(stmts);
        }
        VecStmt(mut stmts) => {
            stmts = stmts.into_iter().map(|stmt| walk(stmt, true)).collect();
            node.op = VecStmt(stmts);
        }
        StmtExpr(body) => {
            node.op = StmtExpr(Box::new(walk(*body, true)));
            node.ty = Box::new(Type::int_ty())
        }
        _ => panic!("unknown node type"),
    };
    node
}

pub fn sema(nodes: Vec<Node>) -> (Vec<Node>, Vec<Var>) {
    let mut new_nodes = vec![];

    for mut node in nodes {
        if let NodeType::Vardef(name, _, Scope::Global(data, len, is_extern)) = node.op {
            let var = Var::new_global(node.ty, name.clone(), data, len, is_extern);
            GLOBALS.lock().unwrap().push(var.clone());
            ENV.lock().unwrap().vars.insert(name, var);
            continue;
        }

        let var;
        match &node.op {
            NodeType::Func(name, _, _, _) | NodeType::Decl(name) => {
                var = Var::new_global(node.ty.clone(), name.clone(), "".into(), 0, false);
                ENV.lock().unwrap().vars.insert(name.clone(), var);
            }
            _ => unreachable!(),
        }

        if matches!(node.op, NodeType::Decl(_)) {
            continue;
        }

        if let NodeType::Func(name, args, body, _) = node.op {
            let mut args2 = vec![];
            for arg in args {
                args2.push(walk(arg, true));
            }
            let body2 = walk(*body, true);
            node.op = NodeType::Func(
                name.clone(),
                args2,
                Box::new(body2),
                *STACKSIZE.lock().unwrap(),
            );
            *STACKSIZE.lock().unwrap() = 0;
            new_nodes.push(node);
        }
    }
    (new_nodes, GLOBALS.lock().unwrap().clone())
}
