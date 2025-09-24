pub mod ospl;
use std::{
    cell::RefCell, env::args, fs::File, io::Read, rc::Rc
};

use crate::ospl::{
    interpreter::Context, parser::Parser, *
};

fn main() {
    runfile("test2.ospl".into(), "block");
}

fn main2() {
    // no clap or anything because I hate myself
    let arg: Vec<String> = args().collect();
    let cmd = &arg.get(1).expect("please specify a command on the command line!");

    match cmd.as_ref() {
        "run" | "exe" | "r" => runfile(
            arg.get(2).expect("please specify a path to the script you wish to run").clone(),
            "block"
        ),
        "run-expr" => runfile(
            arg.get(2).expect("please specify a path to the script you wish to run").clone(),
            "expr"
        ),
        "run-stmt" => runfile(
            arg.get(2).expect("please specify a path to the script you wish to run").clone(),
            "stmt"
        ),
        _ => panic!("unknown command"),
    }

}

fn runfile(path: String, target: &str) {
    // set up
    let ctx: Rc<RefCell<Context>> = Rc::new(RefCell::new(Context::new(None)));
    let p = &mut Parser::new();

    // load file data
    let mut file = File::open(path).expect("file not found!");
    let mut s = String::new();
    file.read_to_string(&mut s).expect("failed to read file");

    // run!
    match target.as_ref() {
        "block" => parser::block(ctx.clone(), p, &s),
        "stmt" => parser::stmt(ctx.clone(), p, &s),
        "expr" => parser::expr(ctx.clone(), p, &s),
        _ => panic!("unknown target")
    }
}