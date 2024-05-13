use clap::Parser as ClapParser;
use codegen::gen_x86;

mod codegen;
mod ir;
mod parse;
mod regalloc;
mod token;

const REGS_N: usize = 8;
const REGS: [&str; REGS_N] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,

    #[arg(long)]
    dump1: bool,
    #[arg(long)]
    dump2: bool,
}

fn main() {
    let args = Args::parse();

    let tokens = token::tokenize(args.input);
    let node = parse::Parser::new(tokens).parse();

    let mut irv = ir::gen_ir(node);

    if args.dump1 {
        ir::dump_ir(&irv);
    }

    regalloc::alloc_regs(&mut irv);

    if args.dump2 {
        ir::dump_ir(&irv);
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    gen_x86(irv)
}
