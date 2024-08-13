use clap::Parser as ClapParser;

mod dump_ir;
mod gen_ir;
mod gen_x86;
mod parse;
mod preprocess;
mod regalloc;
mod sema;
mod token;
mod util;

const REGS_N: usize = 7;

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    path: String,

    #[arg(long)]
    dump_ir1: bool,
    #[arg(long)]
    dump_ir2: bool,
}

fn main() {
    // let args = Args::parse();
    let args = Args {
        path: "test.c".to_string(),
        dump_ir1: false,
        dump_ir2: false,
    };

    let input_string = util::read_file(&args.path);
    let tokens = token::tokenize(
        args.path,
        input_string,
        &mut preprocess::Preprocessor::new(),
    );

    let nodes = parse::parse(&tokens);
    let (nodes, globals) = sema::sema(nodes);
    let mut fns = gen_ir::gen_ir(nodes);

    if args.dump_ir1 {
        dump_ir::dump_ir(&fns);
    }

    regalloc::alloc_regs(&mut fns);

    if args.dump_ir1 {
        dump_ir::dump_ir(&fns);
    }

    gen_x86::gen_x86(globals, fns);
}
