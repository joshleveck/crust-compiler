use crate::gen_ir::{Function, IROp, IRType, IR};
use crate::dump_ir::IRInfo;
use crate::REGS_N;

use lazy_static::lazy_static;
use std::sync::Mutex;


lazy_static! {
    static ref USED: Mutex<[bool; REGS_N]> = Mutex::new([false; REGS_N]);
    static ref REG_MAP: Mutex<[Option<usize>; 8192]> = Mutex::new([None; 8192]);
}

fn used_get(i: usize) -> bool {
    USED.lock().unwrap()[i]
}

fn used_set(i: usize, val: bool) {
    USED.lock().unwrap()[i] = val;
}

fn reg_map_get(i: usize) -> Option<usize> {
    REG_MAP.lock().unwrap().get(i).cloned().unwrap()
}

fn reg_map_set(i: usize, val: usize) {
    REG_MAP.lock().unwrap()[i] = Some(val);
}

fn alloc(ir_reg: usize) -> usize {
    if REG_MAP.lock().unwrap().len() <= ir_reg {
        panic!("program too big");
    }

    if let Some(r) = reg_map_get(ir_reg) {
        assert!(used_get(r));
        return r;
    }

    for i in 0..REGS_N {
        if used_get(i) {
            continue;
        }
        reg_map_set(ir_reg, i);
        used_set(i, true);
        return i;
    }
    panic!("register exhauseted: {}", ir_reg);
}

fn visit(irv: &mut Vec<IR>) {
    use self::IRType::*;

    for item in irv {
        let mut ir = item.clone();
        let info = &IRInfo::from(&ir.op);

        match info.ty {
            Reg | RegImm | RegLabel | LabelAddr => ir.lhs = Some(alloc(ir.lhs.unwrap())),
            Mem | RegReg => {
                ir.lhs = Some(alloc(ir.lhs.unwrap()));
                ir.rhs = Some(alloc(ir.rhs.unwrap()));
            }
            Call => {
                ir.lhs = Some(alloc(ir.lhs.unwrap()));
                match ir.op {
                    IROp::Call(name, nargs, args) => {
                        let mut args_new: [usize; 6] = [0; 6];
                        for i in 0..nargs {
                            args_new[i] = alloc(args[i]);
                        }
                        ir.op = IROp::Call(name, nargs, args_new);
                    }
                    _ => unreachable!(),
                }
            }
            _ => (),
        }

        if ir.op == IROp::Kill {
            let lhs = ir.lhs.unwrap();
            assert!(used_get(lhs));
            used_set(lhs, false);
            ir.op = IROp::Nop;
        }
        *item = ir;
    }
}

pub fn alloc_regs(fns: &mut Vec<Function>) {
    for f in fns {
        *USED.lock().unwrap() = [false; REGS_N];

        visit(&mut f.ir);
    }
}