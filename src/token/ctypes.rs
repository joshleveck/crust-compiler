use super::tokentype::TokenType;


#[derive(Debug, Clone)]
pub struct Type {
    pub ty: Ctype,
    pub size: usize,
    pub align: usize,
}

impl Default for Type {
    fn default() -> Self {
        Type {
            ty: Ctype::Int,
            size: 4,
            align: 4,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ctype {
    Int,
    Char,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Struct(Vec<Node>),
    Void,
    Func(Box<Type>),
}

impl Default for Ctype {
    fn default() -> Self {
        Ctype::Int
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Local(usize),
    Global(String, usize, bool),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ty: Box<Type>,
    pub name: String,
    pub scope: Scope,
}

impl Var {
    pub fn new(ty: Box<Type>, name: String, scope: Scope) -> Self {
        Var { ty, name, scope }
    }

    pub fn new_global(ty: Box<Type>, name: String, data: String, size: usize, is_extern: bool) -> Self {
        Var {
            ty,
            name: name.clone(),
            scope: Scope::Global(data, size, is_extern),
        }
    }
}




#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32),                                        // Number literal
    Str(String, usize),                              // String literal, (data, len)
    Ident(String),                                   // Identifier
    Decl(String),                                    // declaration
    Vardef(String, Option<Box<Node>>, Scope),        // Variable definition, name = init
    Lvar(Scope),                                     // Variable reference
    Gvar(String, String, usize),                     // Variable reference, (name, data, len)
    BinOp(TokenType, Box<Node>, Box<Node>),          // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>),     // "if" ( cond ) then "else" els
    Ternary(Box<Node>, Box<Node>, Box<Node>),        // cond ? then : els
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // "for" ( init; cond; inc ) body
    Break,
    DoWhile(Box<Node>, Box<Node>), // do { body } while(cond)
    Addr(Box<Node>),               // address-of operator("&"), expr
    Deref(Box<Node>),              // pointer dereference ("*"), expr
    Dot(Box<Node>, String, usize), // Struct member accessm, (expr, name, offset)
    Exclamation(Box<Node>),        // !, expr
    Neg(Box<Node>),                // -
    PostInc(Box<Node>),            // post ++
    PostDec(Box<Node>),            // post --
    Return(Box<Node>),             // "return", stmt
    Sizeof(Box<Node>),             // "sizeof", expr
    Alignof(Box<Node>),            // "_Alignof", expr
    Call(String, Vec<Node>),       // Function call(name, args)
    Func(String, Vec<Node>, Box<Node>, usize), // Function definition(name, args, body, stacksize)
    CompStmt(Vec<Node>),           // Compound statement
    VecStmt(Vec<Node>),            // For the purpose of assign a value when initializing an array.
    ExprStmt(Box<Node>),           // Expression statement
    StmtExpr(Box<Node>),           // Statement expression (GNU extn.)
    Null,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub op: NodeType,  // Node type
    pub ty: Box<Type>, // C type
}

impl Node {
    pub fn new(op: NodeType) -> Self {
        Self {
            op,
            ty: Box::new(Type::default()),
        }
    }

    pub fn new_int(val: i32) -> Self {
        Node::new(NodeType::Num(val))
    }

    pub fn scale_ptr(node: Box<Node>, ty: &Type) -> Self {
        match ty.ty {
            Ctype::Ptr(ref ptr_to) => {
                Node::new_binop(TokenType::Mul, *node, Node::new_int(ptr_to.size as i32))
            }
            _ => panic!("expect ptr type"),
        }
    }

    pub fn new_binop(ty: TokenType, lhs: Node, rhs: Node) -> Self {
        Node::new(NodeType::BinOp(ty, Box::new(lhs), Box::new(rhs)))
    }

    pub fn new_num(val: i32) -> Self {
        Node::new(NodeType::Num(val))
    }

    pub fn is_null(&self) -> bool {
        match self.op {
            NodeType::Null => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn new(ty: Ctype, size: usize) -> Self {
        Type {
            ty,
            size,
            align: size,
        }
    }

    pub fn void_ty() -> Self {
        Type::new(Ctype::Void, 0)
    }

    pub fn char_ty() -> Self {
        Type::new(Ctype::Char, 1)
    }

    pub fn int_ty() -> Self {
        Type::new(Ctype::Int, 4)
    }

    pub fn ptr_to(base: Box<Type>) -> Self {
        Type::new(Ctype::Ptr(base), 8)
    }

    pub fn ary_of(base: Box<Type>, len: usize) -> Self {
        let align = base.align;
        let size = base.size * len;
        let mut ty = Type::new(Ctype::Array(base, len), size);
        ty.align = align;
        ty
    }
}