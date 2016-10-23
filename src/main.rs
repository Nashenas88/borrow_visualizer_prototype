#![feature(box_syntax, rustc_private, conservative_impl_trait, rustc_diagnostic_macros)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_borrowck;
extern crate syntax;
extern crate syntax_pos;
extern crate clap;
#[macro_use]
extern crate log;

use rustc::cfg;
use rustc::session::Session;
use rustc::ty::{self, TyCtxt};
use rustc::hir::map as hir_map;
use rustc::hir::map::blocks;
use rustc_borrowck as borrowck;
use rustc_driver::{driver, CompilerCalls, Compilation};
use clap::{Arg, App};

use std::ops::Range;
use std::io::Write;

struct BorrowCalls {
    offset: usize,
    line: Range<usize>
}

impl BorrowCalls {
    fn new(offset: usize, line: Range<usize>) -> BorrowCalls {
        BorrowCalls {
            offset: offset,
            line: line
        }
    }
}

impl<'a> CompilerCalls<'a> for BorrowCalls {
    fn build_controller(&mut self, _: &Session, _: &getopts::Matches) -> driver::CompileController<'a> {
        debug!("build controller..");
        let mut control = driver::CompileController::basic();
        control.after_analysis.stop = Compilation::Stop;

        // reassign so we don't capture self
        let offset = self.offset;
        let line = self.line.clone();
        control.after_analysis.callback = box move |compile_state: &mut driver::CompileState| {
            let tcx = if let Some(tcx) = compile_state.tcx { tcx } else { debug!("no tcx"); return; };
            let (node_id, node, fn_like, fn_node) = nodeid_from_offset_and_line(tcx, offset, &line)
                .expect(&format!("unable to find matching nodeid for offset {} at line {:?}", offset, line));

            let cfg = cfg::CFG::new(tcx, &fn_like.body());
            let (_, analysis_data) = borrowck::build_borrowck_dataflow_data_for_fn(
                tcx,
                compile_state.mir_map,
                fn_like.to_fn_parts(),
                &cfg);
            debug!("Found {} loans within fn identified by {}:\n{:?}\n{:?}", analysis_data.all_loans.len(), node_id.as_u32(), node, fn_node);

            // gather and return analysis data when loan internals can be accessed imm
            println!("[{}]", analysis_data.all_loans.iter()
                // we only care abouts loans related to our target
                .filter(|&loan| loan.loan_path().belongs_to(node_id))
                .map(|loan| {
                    debug!("{:?}", loan);
                    let kind_str = match loan.kind() {
                        ty::BorrowKind::ImmBorrow => "imm",
                        ty::BorrowKind::MutBorrow => "mut",
                        ty::BorrowKind::UniqueImmBorrow => "uimm"
                    };
                    let span = loan.span();
                    // let gen_span = loan.gen_scope().span(&tcx.region_maps, &tcx.map);
                    // let kill_span = loan.kill_scope().span(&tcx.region_maps, &tcx.map);
                    format!("{{\"kind\":\"{}\", \"start\":\"{}\", \"end\":\"{}\"}}",
                        kind_str, span.lo.0, span.hi.0)
                })
                .collect::<Vec<_>>()
                .join(","));
        };

        control
    }
}

fn nodeid_from_offset_and_line<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, offset: usize, line: &Range<usize>)
        -> Option<(syntax::ast::NodeId, hir_map::Node<'tcx>, blocks::FnLikeNode<'tcx>, hir_map::Node<'tcx>)> {
    for (&id, _) in tcx.node_types().iter() {
        let node = if let Some(node) = tcx.map.find(id) {
            node
        } else {
            continue;
        };
        // Avoid statements, they're always ().
        if let hir_map::NodeStmt(_) = node {
            continue;
        }

        if let Some(sp) = tcx.map.opt_span(id) {
            // Avoid peeking at macro expansions.
            if sp.expn_id != syntax_pos::NO_EXPANSION {
                //intln!("In macro");
                continue;
            }

            //intln!("looking at {:?}", node);
            let (lo, hi) = (sp.lo.0 as usize, sp.hi.0 as usize);
            if line.start <= lo && lo <= offset && offset <= hi && hi <= line.end {
                debug!("Looking at nodeid {}", id.as_u32());
                debug!("lo: {}, hi: {}", lo, hi);
                debug!("found matching block: {:?}", node);
                match node {
                    // These cannot be reliably printed.
                    // hir_map::NodeLocal(_) | hir_map::NodeStructCtor(_) => continue,
                    // There is an associated NodeExpr(ExprBlock) where this actually matters.
                    hir_map::NodeBlock(_) => continue,
                    _ => {
                        let mut old_id = id;
                        let mut parent_id = tcx.map.get_parent_node(id);
                        loop {
                            if parent_id == old_id {
                                // reached the root
                                return None;
                            }

                            let parent_node = if let Some(node) = tcx.map.find(parent_id) {
                                node
                            } else {
                                // unable to find node
                                return None;
                            };

                            let code = blocks::Code::from_node(parent_node);
                            if let Some(blocks::FnLikeCode(fn_like)) = code {
                                return Some((id, node, fn_like, parent_node));
                            } else {
                                // we need to jump higher
                                old_id = parent_id;
                                parent_id = tcx.map.get_parent_node(old_id);
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn parse_nums(matches: &clap::ArgMatches) -> Result<(usize, usize, usize), std::num::ParseIntError> {
    let offset = try!(matches.value_of("offset").unwrap().parse::<usize>());
    let line_start = try!(matches.value_of("line_start").unwrap().parse::<usize>());
    let line_end = try!(matches.value_of("line_end").unwrap().parse::<usize>());

    Ok((offset, line_start, line_end))
}

// Takes buffers and never outputs them
struct BlackHole {}

impl Write for BlackHole {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

unsafe impl Send for BlackHole {}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    // collect the program name ahead of time
    let prog = args[0].clone();

    let matches = App::new("Borrow Visualizer")
        .version("0.1")
        .author("Paul D. Faria")
        .about("Poorly finds borrow spans")
        .arg(Arg::with_name("offset")
                .short("o")
                .long("offset")
                .value_name("OFFSET_BYTES")
                .help("The number of bytes from the start of the file to the item to analyze.")
                .takes_value(true)
                .required(true))
        .arg(Arg::with_name("line_start")
                .short("s")
                .long("start")
                .value_name("BYTES")
                .help("The number of bytes from the start of the file to the start of the line of the item to analyze.")
                .takes_value(true)
                .required(true))
        .arg(Arg::with_name("line_end")
                .short("e")
                .long("end")
                .value_name("BYTES")
                .help("The number of bytes from the start of the file to the end of the line of the item to anaylize.")
                .takes_value(true)
                .required(true))
        .setting(clap::AppSettings::TrailingVarArg)
        .arg(Arg::from_usage("<args>... 'args to pass to the compiler'"))
        .get_matches();
    let (offset, line_start, line_end) = match parse_nums(&matches) {
        Ok((o, s, e)) => (o, s, e),
        Err(e) => panic!(e),
    };

    let mut args: Vec<String> = matches.values_of("args").unwrap().map(|s| s.to_string()).collect();
    args.insert(0, prog); // prepend prog back to beginning of args
    rustc_driver::run_compiler(&args, &mut BorrowCalls::new(offset, line_start..line_end), None, Some(box BlackHole{}));
}