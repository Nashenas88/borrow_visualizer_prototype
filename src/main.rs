#![feature(box_syntax, rustc_private, conservative_impl_trait, rustc_diagnostic_macros, str_escape)]

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

use rustc::cfg;
use rustc::session::Session;
use rustc::ty::{self, TyCtxt};
use rustc::hir::map as hir_map;
use rustc::hir::map::blocks;
// use borrowck::{AnalysisData, Loan, LoanPath};
// use borrowck::move_data;
use rustc_driver::{driver, CompilerCalls, Compilation};
use clap::{Arg, App, SubCommand};
use syntax_pos::BytePos;

use std::ops::Range;
use std::io::Write;
use std::fmt::Display;
use std::borrow::Cow;
use log::{LogRecord, LogLevel, LogMetadata};

use std::path::PathBuf;
use rustc::session::config::{self, Input};
use syntax::ast;

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
    fn no_input(&mut self,
                _: &getopts::Matches,
                _: &config::Options,
                _: &ast::CrateConfig,
                _: &Option<PathBuf>,
                _: &Option<PathBuf>,
                _: &errors::registry::Registry)
                -> Option<(Input, Option<PathBuf>)> {
        println!("No input :(");
        None
    }

    fn build_controller(&mut self, _: &Session, _: &getopts::Matches) -> driver::CompileController<'a> {
        trace!("build controller..");
        let mut control = driver::CompileController::basic();
        control.after_analysis.stop = Compilation::Stop;

        // reassign since we can't capture self
        let this = self.clone();
        control.after_analysis.callback = box move |compile_state: &mut driver::CompileState| {
            let tcx = if let Some(tcx) = compile_state.tcx {
                tcx
            } else {
                debug!("no tcx");
                return;
            };

            let (offset, line) = match this.get_byte_info(tcx) {
                Ok(res) => res,
                Err(e) => {
                    print_error(format!("{}", e));
                    return;
                }
            };

            let (node_id, node, fn_like, fn_node) = match nodeid_from_offset_and_line(tcx, offset, &line) {
                Some(res) => res,
                None => {
                    debug!("unable to find matching nodeid for offset {} at line {:?}", offset.0, line);
                    println!("[]");
                    return;
                }
            };

            let body = tcx.hir.krate().body(fn_like.body());
            let cfg = cfg::CFG::new(tcx, &body.value);
            let (_, analysis_data) = borrowck::build_borrowck_dataflow_data_for_fn(
                tcx,
                fn_like.body(),
                &cfg);
            debug!("Found {} loans within fn-like identified by {}:\n{:?}\n{:?}", analysis_data.all_loans.len(), node_id.as_u32(), node, fn_node);

            // In a succesful compilation, `moves` will have 0 or 1 elements. Fortunately,
            // we work with failing compilations too, so it's important that we limit
            // the highlighting of the "live" span to the end of the first move. The goal
            // is to help the user more easily realize that the second move is impossible
            // without the variable being in scope.
            let move_data_moves = analysis_data.move_data.move_data.moves.borrow();
            let moves: Vec<_> = move_data_moves.iter()
                .filter(|m| analysis_data.move_data.move_data.path_loan_path(m.path).belongs_to(node_id))
                .map(|m| tcx.hir.span(m.id))
                .collect();
            let first_move = moves.iter().cloned().min_by_key(|m| m.lo.0);

            // gather and return analysis data when loan internals can be accessed imm
            println!("[{}]", analysis_data.all_loans.iter()
                // we only care abouts loans related to our target
                .filter(|&loan| (*loan.loan_path()).belongs_to(node_id))
                .filter_map(|loan| {
                    debug!("{:?}", loan);
                    let kind_str = match loan.kind() {
                        ty::BorrowKind::ImmBorrow => "imm",
                        ty::BorrowKind::MutBorrow => "mut",
                        ty::BorrowKind::UniqueImmBorrow => "uimm"
                    };

                    // FIXME: do we need this -> let span = loan.span();

                    // Make sure to get the unexpanded span, otherwise we'll
                    // the span of the macro definition in another file! This
                    // leads to some obviously (though it more rarely might not be)
                    // incorrect spans;
                    let gen_span = loan.gen_scope()
                        .span(&tcx.region_maps, &tcx.hir)
                        .map(|s| get_unexpanded_span(s, &tcx));
                    let kill_span = loan.kill_scope()
                        .span(&tcx.region_maps, &tcx.hir)
                        .map(|s| get_unexpanded_span(s, &tcx));

                    if let (Some(gen_span), Some(kill_span)) = (gen_span, kill_span) {
                        let borrow_span = syntax_pos::Span{
                            lo: gen_span.lo,
                            hi: kill_span.hi,
                            expn_id: syntax_pos::NO_EXPANSION
                        };
                        Some(this.print_borrow(tcx, kind_str, borrow_span))
                    } else {
                        debug!("One of gen_span or kill_span does not exist for loan");
                        None
                    }
                })
                .chain(moves.iter().map(|move_span| this.print_borrow(tcx, "mov", *move_span)))
                .chain(analysis_data.move_data.move_data.var_assignments.borrow().iter()
                    .filter(|a| a.assignee_id == node_id)
                    .filter_map(|a| {
                        // we may want to visualiza a.span in some way
                        let path = analysis_data.move_data.move_data.path_loan_path(a.path);
                        let kill_span = path.kill_scope(tcx).span(&tcx.region_maps, &tcx.hir);
                        if let Some(kill_span) = kill_span {
                            // If there's a move span, then we limit the "live" span to the end
                            // of the move span. This shows that the variable's "scope" has
                            // ended at this point. Any usages (reads, writes, borrows, moves)
                            // can still be visualized, but should match with compiler errors.
                            let hi = if let Some(move_span) = first_move {
                                move_span.hi
                            } else {
                                kill_span.hi
                            };
                            let live_span = syntax_pos::Span{ lo: kill_span.lo, hi: hi, expn_id: syntax_pos::NO_EXPANSION };
                            Some(this.print_borrow(tcx, "live", live_span))
                        } else {
                            debug!("kill_span does not exist for var");
                            None
                        }
                    }))
                .collect::<Vec<_>>()
                .join(","));
        };

        control
    }
}

fn get_unexpanded_span<'a, 'tcx>(input_span: syntax_pos::Span, tcx: &TyCtxt<'a, 'tcx, 'tcx>) -> syntax_pos::Span {
    let cm = tcx.sess.codemap();
    // Walk up the macro expansion chain until we reach a non-expanded span.
    let mut span = input_span;
    while span.expn_id != syntax_pos::NO_EXPANSION && span.expn_id != syntax_pos::COMMAND_LINE_EXPN {
        if let Some(callsite_span) = cm.with_expn_info(span.expn_id,
                                            |ei| ei.map(|ei| ei.call_site.clone())) {
            span = callsite_span;
        } else {
            break;
        }
    }

    span
}

fn nodeid_from_offset_and_line<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, offset: BytePos, line: &Range<BytePos>)
        -> Option<(syntax::ast::NodeId, hir_map::Node<'tcx>, blocks::FnLikeNode<'tcx>, hir_map::Node<'tcx>)> {
    debug!("Searching for node");
    for def_id in tcx.tables.borrow().keys() {
        let tables = tcx.tables.borrow(); // satisfy borrow checker
        let entry = tables.get(&def_id).unwrap();
        let def_span = match tcx.hir.span_if_local(def_id) {
            Some(sp) => sp,
            None => continue,
        };

        if def_span.expn_id != syntax_pos::NO_EXPANSION {
            continue;
        }

        let (lo, hi) = (def_span.lo.0 as u32, def_span.hi.0 as u32);
        // may fail if we don't take offset into account, e.g. def starts in the middle of the line
        if !(lo <= offset.0 && offset.0 <= hi) {
            // The line we're looking for is not in this def
            continue;
        }

        debug!("Found potential matching def! {:?}", def_id);
        debug!("Def Span: {}-{}", lo, hi);
        debug!("Cursor: {}", offset.0);

        for (&id, _) in entry.node_types.iter() {
            debug!("id: {} => ", id);
            let node = if let Some(node) = tcx.hir.find(id) {
                node
            } else {
                debug!("None");
                continue;
            };

            // the important parts exists as other nodes
            if let hir_map::NodeStmt(..) = node {
                continue;
            }

            let sp = tcx.hir.span(id);
            // Avoid peeking at macro expansions.
            if sp.expn_id != syntax_pos::NO_EXPANSION {
                continue;
            }

            let (lo, hi) = (sp.lo.0 as u32, sp.hi.0 as u32);
            if line.start.0 <= lo && lo <= offset.0
                    && offset.0 <= hi && hi <= line.end.0 {
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
                        let mut parent_id = tcx.hir.get_parent_node(id);
                        loop {
                            if parent_id == old_id {
                                // reached the root
                                continue;
                            }

                            let parent_node = if let Some(node) = tcx.hir.find(parent_id) {
                                node
                            } else {
                                // unable to find node
                                continue;
                            };

                            let code = blocks::Code::from_node(&tcx.hir, parent_id);
                            if let Some(blocks::Code::FnLike(fn_like)) = code {
                                return Some((id, node, fn_like, parent_node));
                            } else {
                                // we need to jump higher
                                old_id = parent_id;
                                parent_id = tcx.hir.get_parent_node(old_id);
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
    StrKind(Cow<'a,str>),
    SetLoggerError(log::SetLoggerError),
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

impl<'a> From<log::SetLoggerError> for ParseError<'a> {
    fn from(e: log::SetLoggerError) -> Self {
        ParseError::SetLoggerError(e)
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &ParseError::ParseIntKind(ref e) => e.fmt(f),
            &ParseError::StrKind(ref e) => write!(f, "{}", e),
            &ParseError::SetLoggerError(ref e) => e.fmt(f),
        }
    }
}

fn parse_compiler_args<'a: 'b, 'b, F>(matches: &'a clap::ArgMatches, f: F) -> Result<(BorrowCalls<'b>, Vec<String>), ParseError<'b>>
    where F: FnOnce(Vec<String>) -> (BorrowCalls<'b>, Vec<String>)
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
            if let Some(output) = sub_m.value_of("log_out") {
                try!(log::set_logger(move |_| {
                    let file = std::fs::OpenOptions::new()
                        .read(true)
                        .write(true)
                        .create(true)
                        .open(output);
                    box SimpleFileLogger::new(file.unwrap())
                }));
            }

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
            if let Some(output) = sub_m.value_of("log_out") {
                try!(log::set_logger(move |max_log_level| {
                    max_log_level.set(log::LogLevelFilter::Debug);
                    let file = std::fs::OpenOptions::new()
                        .read(true)
                        .append(true)
                        .create(true)
                        .open(output);
                    box SimpleFileLogger::new(file.unwrap())
                }));
            }

            parse_compiler_args(sub_m, |mut args| {
                let borrow_calls = BorrowCalls::with_line_info(file_name, line, column);
                args.push(file_name.to_owned());
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
    app.arg_from_usage("[args]... 'Arguments to pass to the compiler'")
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
                .required(true))
            .arg(Arg::with_name("log_out")
                .value_name("OUTPUT_FILE")
                .help("The file to write logs to.")
                .takes_value(true)
                .required(false))))
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
                .required(true))
            .arg(Arg::with_name("log_out")
                .value_name("OUTPUT_FILE")
                .help("The file to write logs to.")
                .takes_value(true)
                .required(false))))
        .get_matches()
}

fn gen_error<'a, T>(message: T) -> String where T: Into<Cow<'a, str>> {
    let message = message.into().to_owned().escape_default();
    format!("[{{\"kind\":\"error\",\"error\":\"{}\"}}]", message)
}

fn print_error<'a, T>(message: T) where T: Into<Cow<'a, str>> {
    println!("{}", gen_error(message));
}

struct SimpleFileLogger {
    file: std::sync::Mutex<std::cell::RefCell<std::fs::File>>
}

impl SimpleFileLogger {
    fn new(file: std::fs::File) -> Self {
        SimpleFileLogger { file: std::sync::Mutex::new(std::cell::RefCell::new(file)) }
    }
}

impl log::Log for SimpleFileLogger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= LogLevel::Debug
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            let file = self.file.lock().unwrap();
            if let Err(e) = write!(file.borrow_mut(), "{}\n", record.args()) {
                if let Err(e) = write!(std::io::stderr(), "Failed to write to log file: {}", e) {
                    panic!("Something went horribly, horribly wrong: {}", e);
                }
            };
        }
    }
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
