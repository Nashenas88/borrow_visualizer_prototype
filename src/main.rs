#![feature(box_syntax, rustc_private, conservative_impl_trait, rustc_diagnostic_macros, str_escape)]

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
use clap::{Arg, App, SubCommand};
use syntax_pos::BytePos;

use std::ops::Range;
use std::io::Write;
use std::fmt::Display;
use std::borrow::Cow;

#[derive(Clone)]
enum InputKind<'a> {
    Bytes(BytePos, Range<BytePos>),
    LineInfo(&'a str, u32, u32)
}

#[derive(Clone)]
struct BorrowCalls<'a> {
    input: InputKind<'a>
}

impl<'a> BorrowCalls<'a> {
    fn with_bytes(offset: BytePos, line: Range<BytePos>) -> Self {
        BorrowCalls {
            input: InputKind::Bytes(offset, line)
        }
    }

    fn with_line_info(file_name: &'a str, line: u32, column: u32) -> Self {
        BorrowCalls {
            input: InputKind::LineInfo(file_name, line, column)
        }
    }

    fn get_byte_info<'t, 'tcx>(&self, tcx: TyCtxt<'t, 'tcx, 'tcx>) -> Result<(BytePos, Range<BytePos>), String> {
        match self.input {
            InputKind::Bytes(offset, ref line) => Ok((offset, line.clone())),
            InputKind::LineInfo(file_name, line, column) => {
                let file_map = match tcx.sess.codemap().get_filemap(file_name) {
                    Some(fm) => fm,
                    None => {
                        return Err(gen_error("filename not found"));
                    }
                };

                let line_idx = line as usize;
                let file_lines = file_map.lines.borrow();
                let line_start = file_lines[line_idx];
                let line_end = if line_idx >= file_lines.len() {
                    file_map.end_pos
                } else {
                    match file_lines[line_idx + 1] {
                        BytePos(i) => BytePos(i - 1)
                    }
                };
                let offset = match line_start {
                    BytePos(i) => BytePos(i + column)
                };

                Ok((offset, line_start..line_end))
            }
        }
    }

    fn print_borrow<'t, 'tcx>(&self, tcx: TyCtxt<'t, 'tcx, 'tcx>, kind_str: &str, span: syntax_pos::Span) -> String {
        match self.input {
            InputKind::Bytes(..) => format!("{{\"kind\":\"{}\", \"start\":\"{}\", \"end\":\"{}\"}}",
                kind_str, span.lo.0, span.hi.0),
            InputKind::LineInfo(..) => {
                let loc_start = tcx.sess.codemap().lookup_char_pos(span.lo);
                let loc_end = tcx.sess.codemap().lookup_char_pos(span.hi);
                format!("{{\"kind\":\"{}\",\"start\":{{\"line\":{},\"col\":{}}},\"end\":{{\"line\":{},\"col\":{}}}}}",
                    kind_str, loc_start.line, loc_start.col.0, loc_end.line, loc_end.col.0)
            }
        }
    }
}

impl<'a, 'b: 'a> CompilerCalls<'a> for BorrowCalls<'b> {
    fn build_controller(&mut self, _: &Session, _: &getopts::Matches) -> driver::CompileController<'a> {
        debug!("build controller..");
        let mut control = driver::CompileController::basic();
        control.after_analysis.stop = Compilation::Stop;

        // reassign since we can't capture self
        let this = self.clone();
        control.after_analysis.callback = box move |compile_state: &mut driver::CompileState| {
            let tcx = if let Some(tcx) = compile_state.tcx { tcx } else { debug!("no tcx"); return; };
            let (offset, line) = match this.get_byte_info(tcx) {
                Ok((o, l)) => (o, l),
                Err(e) => {
                    print_error(format!("{}", e));
                    return;
                }
            };

            let (node_id, node, fn_like, fn_node) = nodeid_from_offset_and_line(tcx, offset, &line)
                .expect(&format!("unable to find matching nodeid for offset {} at line {:?}", offset.0, line));

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
                    // let span = loan.span();
                    let gen_span = loan.gen_scope().span(&tcx.region_maps, &tcx.map);
                    let kill_span = loan.kill_scope().span(&tcx.region_maps, &tcx.map);
                    match (gen_span, kill_span) {
                        (Some(gen_span), Some(kill_span)) => {
                            let borrow_span = syntax_pos::Span{ lo: gen_span.lo, hi: kill_span.hi, expn_id: syntax_pos::NO_EXPANSION };
                            this.print_borrow(tcx, kind_str, borrow_span)
                        }
                        _ => {
                            debug!("One of gen_span or kill_span does not exist for loan");
                            "".to_owned()
                        }
                    }
                })
                .filter(|json_str| json_str != "")
                .collect::<Vec<_>>()
                .join(","));
        };

        control
    }
}

fn nodeid_from_offset_and_line<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, offset: BytePos, line: &Range<BytePos>)
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
                continue;
            }

            let (lo, hi) = (sp.lo.0 as u32, sp.hi.0 as u32);
            if line.start.0 <= lo && lo <= offset.0 && offset.0 <= hi && hi <= line.end.0 {
                debug!("Looking at nodeid {}", id.as_u32());
                debug!("lo: {}, hi: {}", lo, hi);
                debug!("found matching block: {:?}", node);
                match node {
                    // These cannot be reliably printed.
                    // hir_map::NodeLocal(_) | hir_map::NodeStructCtor(_) => continue,
                    // There is an associated NodeExpr(ExprBlock) where this actually matters.
                    hir_map::NodeBlock(_) => continue,
                    // TODO Needs to be narrowed down more
                    _ => {
                        let mut old_id = id;
                        let mut parent_id = tcx.map.get_parent_node(id);
                        loop {
                            if parent_id == old_id {
                                // reached the root
                                continue;
                            }

                            let parent_node = if let Some(node) = tcx.map.find(parent_id) {
                                node
                            } else {
                                // unable to find node
                                continue;
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

#[derive(Debug)]
enum ParseError<'a> {
    ParseIntKind(std::num::ParseIntError),
    StrKind(Cow<'a,str>)
}

impl<'a> From<std::num::ParseIntError> for ParseError<'a> {
    fn from(e: std::num::ParseIntError) -> Self {
        ParseError::ParseIntKind(e)
    }
}

impl<'a> From<&'a str> for ParseError<'a> {
    fn from(e: &'a str) -> Self {
        ParseError::StrKind(Cow::Borrowed(e))
    }
}

impl<'a> From<String> for ParseError<'a> {
    fn from(e: String) -> Self {
        ParseError::StrKind(Cow::Owned(e))
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &ParseError::ParseIntKind(ref e) => e.fmt(f),
            &ParseError::StrKind(ref e) => write!(f, "{}", e)
        }
    }
}

fn parse_compiler_args<'a: 'b, 'b, F>(matches: &'a clap::ArgMatches, f: F) -> Result<(BorrowCalls<'b>, Vec<String>), ParseError<'b>>
    where F: Fn(Vec<String>) -> (BorrowCalls<'b>, Vec<String>)
{
    matches.values_of("args")
        .map(|args| f(args.map(str::to_string).collect()))
        .ok_or(gen_error("Compile args are missing").into())
}

fn parse_input<'a: 'b, 'b>(matches: &'a clap::ArgMatches) -> Result<(BorrowCalls<'b>, Vec<String>), ParseError<'b>> {
    match matches.subcommand() {
        ("bytes", Some(sub_m)) => {
            let offset = try!(sub_m.value_of("offset").unwrap().parse::<u32>());
            let line_start = try!(sub_m.value_of("line_start").unwrap().parse::<u32>());
            let line_end = try!(sub_m.value_of("line_end").unwrap().parse::<u32>());
            parse_compiler_args(sub_m, |args| {
                let line = BytePos(line_start)..BytePos(line_end);
                let borrow_calls = BorrowCalls::with_bytes(BytePos(offset), line);
                (borrow_calls, args)
            })
        },
        ("line", Some(sub_m)) => {
            let file_name = sub_m.value_of("file_name").unwrap();
            let line = try!(sub_m.value_of("line").unwrap().parse::<u32>());
            let column = try!(sub_m.value_of("column").unwrap().parse::<u32>());
            parse_compiler_args(sub_m, |args| {
                let borrow_calls = BorrowCalls::with_line_info(file_name, line, column);
                (borrow_calls, args)
            })
        },
        (cmd, _) => Err(gen_error(format!("Unrecognized subcommand: {}", cmd)).into())
    }
}

// Takes buffers and never outputs them
struct BlackHole {}

impl Write for BlackHole {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        // pretend we wrote everything or the compiler will error out
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

unsafe impl Send for BlackHole {}

fn extend_args<'a>(app: App<'a, 'a>) -> App<'a, 'a> {
    app.setting(clap::AppSettings::TrailingVarArg)
        .arg(Arg::from_usage("<args>... 'arguments to pass to the compiler.'"))
}

fn get_matches<'a>() -> clap::ArgMatches<'a> {
    App::new("Borrow Visualizer")
        .version("0.1")
        .author("Paul D. Faria")
        .about("Poorly finds borrow spans")
        .subcommand(extend_args(SubCommand::with_name("bytes")
            .arg(Arg::with_name("offset")
                    .value_name("OFFSET_BYTES")
                    .help("The number of bytes from the start of the file to the item to analyze.")
                    .takes_value(true)
                    .required(true))
            .arg(Arg::with_name("line_start")
                    .value_name("LINE_START_BYTES")
                    .help("The number of bytes from the start of the file to the start of the line of the item to analyze.")
                    .takes_value(true)
                    .required(true))
            .arg(Arg::with_name("line_end")
                    .value_name("LINE_END_BYTES")
                    .help("The number of bytes from the start of the file to the end of the line of the item to anaylize.")
                    .takes_value(true)
                    .required(true))))
        .subcommand(extend_args(SubCommand::with_name("line")
            .arg(Arg::with_name("file_name")
                .value_name("FILE_NAME")
                .help("The name of the file that's being analyzed.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("line")
                .value_name("LINE_NUMBER")
                .help("The line number for the item to analyze.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("column")
                .value_name("COLUMN_NUMBER")
                .help("The column number for the item to analyze.")
                .takes_value(true)
                .required(true))))
        .get_matches()
}

fn gen_error<'a, T>(message: T) -> String where T: Into<Cow<'a, str>> {
    let message = message.into().to_owned().escape_default();
    format!("[{{\"error\":\"{}\"}}]", message)
}

fn print_error<'a, T>(message: T) where T: Into<Cow<'a, str>> {
    println!("{}", gen_error(message));
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    // collect the program name ahead of time
    let prog = args[0].clone();

    let matches = get_matches();
    let (mut borrow_calls, mut args) = match parse_input(&matches) {
        Ok(bc) => bc,
        Err(e) => {
            print_error(format!("{}", e));
            return;
        },
    };

    args.insert(0, prog); // prepend program name back to beginning of remaining args
    rustc_driver::run_compiler(&args, &mut borrow_calls, None, Some(box BlackHole{}));
}