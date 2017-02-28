use rustc::ty::TyCtxt;
use syntax_pos::Span;
use std::cmp::Ordering;

pub enum Region {
    Byte(ByteRegion),
    Line(LineRegion),
}

impl PartialEq for Region {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Region::Byte(ref me), &Region::Byte(ref other)) => me.eq(other),
            (&Region::Line(ref me), &Region::Line(ref other)) => me.eq(other),
            _ => false,
        }
    }
}

impl Eq for Region { }

impl PartialOrd for Region {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (&Region::Byte(ref me), &Region::Byte(ref other)) => me.partial_cmp(other),
            (&Region::Line(ref me), &Region::Line(ref other)) => me.partial_cmp(other),
            _ => None,
        }
    }
}

impl Ord for Region {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (&Region::Byte(ref me), &Region::Byte(ref other)) => me.cmp(other),
            (&Region::Line(ref me), &Region::Line(ref other)) => me.cmp(other),
            _ => unreachable!(),
        }
    }
}

#[derive(Serialize)]
pub struct ByteRegion {
    kind: Kind,
    start: usize,
    end: usize,
}

impl ByteRegion {
    pub fn new(kind: Kind, span: Span) -> Self {
        ByteRegion {
            kind: kind,
            start: span.lo.0 as usize,
            end: span.hi.0 as usize,
        }
    }
}

impl PartialEq for ByteRegion {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start
    }
}

impl Eq for ByteRegion { }

impl PartialOrd for ByteRegion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ByteRegion {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.start == other.start {
            Ordering::Equal
        } else if self.start < other.start {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

#[derive(Serialize)]
pub struct LinePosition {
    line: usize,
    col: usize,
}

#[derive(Serialize)]
pub struct LineRegion {
    kind: Kind,
    start: LinePosition,
    end: LinePosition,
}

impl PartialEq for LineRegion {
    fn eq(&self, other: &Self) -> bool {
        self.start.line == other.start.line
            && self.start.col == other.start.col
    }
}

impl Eq for LineRegion { }

impl PartialOrd for LineRegion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LineRegion {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.start.line == other.start.line {
            if self.start.col == other.start.col {
                Ordering::Equal
            } else if self.start.col < other.start.col {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        } else if self.start.line < other.start.line {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

impl LineRegion {
    pub fn new<'t, 'tcx>(tcx: TyCtxt<'t, 'tcx, 'tcx>, kind: Kind, span: Span) -> Self {
        let loc_start = tcx.sess.codemap().lookup_char_pos(span.lo);
        let loc_end = tcx.sess.codemap().lookup_char_pos(span.hi);
        LineRegion {
            kind: kind,
            start: LinePosition {
                line: loc_start.line,
                col: loc_start.col.0,
            },
            end: LinePosition {
                line: loc_end.line,
                col: loc_end.col.0,
            }
        }
    }
}

#[derive(Serialize)]
pub enum Kind {
    #[serde(rename = "assign")]
    Assign,

    #[serde(rename = "live")]
    Live,

    #[serde(rename = "imm")]
    Immutable,

    #[serde(rename = "uimm")]
    UniqueImmutable,

    #[serde(rename = "mut")]
    Mutable,

    #[serde(rename = "mov")]
    Move
}