#![feature(
    box_syntax,
    rustc_private,
    conservative_impl_trait,
    rustc_diagnostic_macros,
    str_escape)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_borrowck as borrowck;
extern crate rustc_errors as errors;
extern crate syntax;
extern crate syntax_pos;
extern crate clap;
#[macro_use]
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

mod borrowcalls;
mod blackhole;
mod region;
mod args;
mod error;

use blackhole::BlackHole;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    // collect the program name ahead of time
    let prog = args[0].clone();

    let matches = args::get_matches();
    let (mut borrow_calls, mut args) = match args::parse_input(&matches) {
        Ok(bc) => bc,
        Err(e) => {
            error::print_error(format!("{}", e));
            return;
        },
    };

    // Prepend program name back to beginning of remaining args
    // so we can pass it to to the driver.
    args.insert(0, prog); 
    rustc_driver::run_compiler(&args, &mut borrow_calls, None, Some(box BlackHole{}));
}
