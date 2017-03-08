use std::borrow::Cow;
use std::fmt::{self, Display};
use std::num;
use std::ops::Range;
use std::path::PathBuf;

use borrowck;
use errors;
use getopts;
use log;
use syntax;
use error;

use rustc::cfg;
use rustc::hir::{self, map as hir_map};
use rustc::hir::map::blocks;
use rustc::session::config::{self, Input};
use rustc::session::Session;
use rustc::ty::{self, TyCtxt};
use rustc_driver::{driver, CompilerCalls, Compilation};
use region::{Region, LineRegion, ByteRegion, Kind};
use serde_json;
use syntax::ast;
use syntax_pos::{self, BytePos, Span};

#[derive(Clone)]
enum InputKind<'a> {
    Bytes(BytePos, Range<BytePos>),
    LineInfo(&'a str, u32, u32)
}

#[derive(Clone)]
pub struct BorrowCalls<'a> {
    input: InputKind<'a>
}

impl<'a> BorrowCalls<'a> {
    pub fn with_bytes(offset: BytePos, line: Range<BytePos>) -> Self {
        BorrowCalls {
            input: InputKind::Bytes(offset, line)
        }
    }

    pub fn with_line_info(file_name: &'a str, line: u32, column: u32) -> Self {
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
                        return Err(error::gen_error("filename not found"));
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

    fn get_region_constructor(&self) -> Box<for <'t, 'tcx> Fn(TyCtxt<'t, 'tcx, 'tcx>, Kind, Span) -> Region> {
        match self.input {
            InputKind::Bytes(..) => box |_, kind, span| Region::Byte(ByteRegion::new(kind, span)),
            InputKind::LineInfo(..) => box |tcx, kind, span| Region::Line(LineRegion::new(tcx, kind, span)),
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

        let region_constructor = self.get_region_constructor();
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
                    error::print_error(format!("{}", e));
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
            let cfg = cfg::CFG::new(tcx, &body);
            let (_, analysis_data) = borrowck::build_borrowck_dataflow_data_for_fn(
                tcx,
                fn_like.body(),
                &cfg);
            // debug!("Found {} loans within fn-like identified by {}:\n{:?}\n{:?}", analysis_data.all_loans.len(), node_id.as_u32(), node, fn_node);

            // In a succesful compilation, `moves` will have 0 or 1 elements. Fortunately,
            // we work with failing compilations too, so it's important that we limit
            // the highlighting of the "live" span to the end of the first move. The goal
            // is to help the user more easily realize that the second move is impossible.
            let move_data = analysis_data.move_data.move_data;
            let move_data_moves = move_data.moves.borrow();
            let mut moves: Vec<_> = move_data_moves.iter()
                .filter(|m| move_data.path_loan_path(m.path).belongs_to(node_id))
                .map(|m| tcx.hir.span(m.id))
                .collect();
            moves.sort();
            let first_move = if moves.len() > 0 { Some(moves[0]) } else { None };

            // gather and return analysis data when loan internals can be accessed imm
            let mut regions = move_data.var_assignments.borrow().iter()
                .filter(|a| a.assignee_id == node_id)
                .filter_map(|a| {
                    // we may want to visualiza a.span in some way
                    let path = move_data.path_loan_path(a.path);
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
                        let live_span = Span{ lo: kill_span.lo, hi: hi, expn_id: syntax_pos::NO_EXPANSION };
                        Some(region_constructor(tcx, Kind::Live, live_span))
                    } else {
                        debug!("kill_span does not exist for var");
                        None
                    }
                })
                .chain(analysis_data.all_loans.iter()
                    // we only care abouts loans related to our target
                    .filter(|&loan| (*loan.loan_path()).belongs_to(node_id))
                    .filter_map(|loan| {
                        debug!("{:?}", loan);
                        let kind = match loan.kind() {
                            ty::BorrowKind::ImmBorrow => Kind::Immutable,
                            ty::BorrowKind::MutBorrow => Kind::Mutable,
                            ty::BorrowKind::UniqueImmBorrow => Kind::UniqueImmutable,
                        };

                        // FIXME: do we need this -> let span = loan.span();

                        // Make sure to get the unexpanded span, otherwise we'll
                        // the span of the macro definition in another file! This
                        // leads to some obviously (though it more rarely might not be)
                        // incorrect spans;
                        let gen_span = loan.gen_scope()
                            .span(&tcx.region_maps, &tcx.hir)
                            .and_then(|s| get_unexpanded_span(s, &tcx));
                        let kill_span = loan.kill_scope()
                            .span(&tcx.region_maps, &tcx.hir)
                            .and_then(|s| get_unexpanded_span(s, &tcx));

                        if let (Some(gen_span), Some(kill_span)) = (gen_span, kill_span) {
                            let borrow_span = syntax_pos::Span{
                                lo: gen_span.lo,
                                hi: kill_span.hi,
                                expn_id: syntax_pos::NO_EXPANSION
                            };
                            Some(region_constructor(tcx, kind, borrow_span))
                        } else {
                            debug!("One of gen_span or kill_span does not exist for loan");
                            None
                        }
                    }))
                .chain(moves.iter().map(|s| region_constructor(tcx, Kind::Move, *s)))
                .collect::<Vec<_>>();
            regions.push(region_constructor(tcx, Kind::Assign, tcx.hir.span(node_id)));
            regions.sort();
            println!("[{}]", regions.iter().map(|r| match *r {
                    Region::Byte(ref r) => serde_json::to_string(r),
                    Region::Line(ref r) => serde_json::to_string(r),
                })
                .map(|serialize_result| serialize_result.unwrap_or_else(|e| panic!("failed to serialize: {:?}", e)))
                .collect::<Vec<_>>()
                .join(","));
        };

        control
    }
}

fn get_unexpanded_span<'a, 'tcx>(input_span: syntax_pos::Span, tcx: &TyCtxt<'a, 'tcx, 'tcx>) -> Option<syntax_pos::Span> {
    let cm = tcx.sess.codemap();
    // Walk up the macro expansion chain until we reach a non-expanded span.
    let mut span = input_span;
    while span.expn_id != syntax_pos::NO_EXPANSION && span.expn_id != syntax_pos::COMMAND_LINE_EXPN {
        if let Some(callsite_span) = cm.with_expn_info(span.expn_id,
                                            |ei| ei.map(|ei| ei.call_site.clone())) {
            span = callsite_span;
        } else {
            // There are no unexpanded spans, so we can't generate a span
            // for the current file.
            return None;
        }
    }

    Some(span)
}

fn nodeid_from_offset_and_line<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, offset: BytePos, line: &Range<BytePos>)
        -> Option<(syntax::ast::NodeId, Option<hir_map::Node<'tcx>>, blocks::FnLikeNode<'tcx>, hir_map::Node<'tcx>)> {
    debug!("Searching for node");
    for def_id in tcx.maps.typeck_tables.borrow().keys() {
        let tables = tcx.maps.typeck_tables.borrow();
        let entry = tables.get(&def_id).unwrap();
        let def_span = match tcx.hir.span_if_local(def_id) {
            Some(sp) => sp,
            None => continue,
        };

        if def_span.expn_id != syntax_pos::NO_EXPANSION {
            continue;
        }

        if !within_def_span(def_span, offset) {
            // The line we're looking for is not in this def
            continue;
        }

        debug!("Found potential matching def! {:?}", def_id);
        debug!("Def Span: {:?}", def_span);
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

            if !within_span(sp, offset, line) {
                continue;
            }

            debug!("Looking at nodeid {}", id.as_u32());
            debug!("found matching block: {:?}", node);
            match node {
                // These cannot be reliably printed.
                // hir_map::NodeLocal(_) | hir_map::NodeStructCtor(_) => continue,
                // There is an associated NodeExpr(ExprBlock) where this actually matters.
                hir_map::NodeBlock(_) => continue,
                hir_map::NodePat(ref pat) => {
                    let def_id = filter_pattern(pat, offset, line);
                    if def_id.is_none() {
                        continue;
                    }

                    let def_id = def_id.unwrap();
                    let entry = tables.get(&def_id);
                    if entry.is_none() {
                        debug!("None entry :(");
                        continue;
                    }

                    let entry = entry.unwrap();
                    for (&id, _) in entry.node_types.iter() {
                        let node = tcx.hir.find(id);
                        debug!("Node for pattern: {:?}", node);
                        if let tup @ Some(..) = get_fn_node(tcx, id, node) {
                            return tup;
                        }
                    }
                },
                // TODO Needs to be narrowed down more
                _ => {
                    if let tup @ Some(..) = get_fn_node(tcx, id, Some(node)) {
                        return tup;
                    }
                }
            }
        }
    }
    None
}

fn get_fn_node<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, id: syntax::ast::NodeId, node: Option<hir_map::Node<'tcx>>)
        -> Option<(syntax::ast::NodeId, Option<hir_map::Node<'tcx>>, blocks::FnLikeNode<'tcx>, hir_map::Node<'tcx>)> {
    let mut old_id = id;
    let mut parent_id = tcx.hir.get_parent_node(id);
    loop {
        if parent_id == old_id {
            // reached the root
            return None;
        }

        let parent_node = if let Some(node) = tcx.hir.find(parent_id) {
            node
        } else {
            // unable to find node
            return None;
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

fn within_span(span: Span, offset: BytePos, line: &Range<BytePos>) -> bool {
    let (lo, hi) = (span.lo.0 as u32, span.hi.0 as u32);
    line.start.0 <= lo && lo <= offset.0
        && offset.0 <= hi && hi <= line.end.0
}

fn within_def_span(span: Span, offset: BytePos) -> bool {
    let (lo, hi) = (span.lo.0 as u32, span.hi.0 as u32);
    lo <= offset.0 && offset.0 <= hi
}

fn filter_pattern(pat: &hir::Pat, offset: BytePos, line: &Range<BytePos>) -> Option<hir::def_id::DefId> {
    match &pat.node {
        &hir::PatKind::Binding(_, def_id, spanned, ref opt_pat) => {
            if within_span(spanned.span, offset, line) {
                Some(def_id)
            } else {
                opt_pat.as_ref().and_then(|pat| filter_pattern(pat, offset, line))
            }
        },
        &hir::PatKind::Struct(_, ref field_paths, ..) => {
            for spanned_field_pat in field_paths.as_ref().iter() {
                if within_span(spanned_field_pat.span, offset, line) {
                    let node = &spanned_field_pat.node;
                    let pat = &node.pat;
                    return filter_pattern(pat, offset, line);
                }
            }
            None
        },
        &hir::PatKind::TupleStruct(_, ref pats, ..)
        | &hir::PatKind::Tuple(ref pats, ..) => {
            for pat in pats {
                let def_id = filter_pattern(pat, offset, line);
                if def_id.is_some() {
                    return def_id;
                }
            }
            None
        },
        &hir::PatKind::Box(ref pat) => filter_pattern(pat, offset, line),
        &hir::PatKind::Ref(ref pat, ..) => filter_pattern(pat, offset, line),
        &hir::PatKind::Slice(ref start_pats, ref opt_middle_pat, ref end_pats) => {
            let pats = start_pats.iter()
                .chain(opt_middle_pat.iter())
                .chain(end_pats.iter());
            for pat in pats {
                let def_id = filter_pattern(pat, offset, line);
                if def_id.is_some() {
                    return def_id;
                }
            }
            None
        },
        _ => None,
    }
}

#[derive(Debug)]
pub enum ParseError<'a> {
    ParseIntKind(num::ParseIntError),
    StrKind(Cow<'a,str>),
    SetLoggerError(log::SetLoggerError),
}

impl<'a> From<num::ParseIntError> for ParseError<'a> {
    fn from(e: num::ParseIntError) -> Self {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::ParseIntKind(ref e) => e.fmt(f),
            &ParseError::StrKind(ref e) => write!(f, "{}", e),
            &ParseError::SetLoggerError(ref e) => e.fmt(f),
        }
    }
}
