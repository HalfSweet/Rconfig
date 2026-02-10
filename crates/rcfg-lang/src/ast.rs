use crate::span::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Use(UseStmt),
    Mod(ModDecl),
    Enum(EnumDecl),
    Option(OptionDecl),
    Export(ExportBlock),
    Patch(PatchBlock),
    Require(RequireStmt),
    Constraint(ConstraintBlock),
    When(WhenBlock),
    Match(MatchBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemMeta {
    pub attrs: Vec<Attr>,
    pub doc: Vec<Spanned<String>>,
}

impl ItemMeta {
    pub fn empty() -> Self {
        Self {
            attrs: Vec::new(),
            doc: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseStmt {
    pub meta: ItemMeta,
    pub path: Path,
    pub alias: Option<Spanned<String>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModDecl {
    pub meta: ItemMeta,
    pub name: Spanned<String>,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub meta: ItemMeta,
    pub name: Spanned<String>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub meta: ItemMeta,
    pub name: Spanned<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OptionDecl {
    pub meta: ItemMeta,
    pub name: Spanned<String>,
    pub ty: Type,
    pub default: Option<ConstValue>,
    pub attached_constraints: Option<OptionAttachedConstraints>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportBlock {
    pub meta: ItemMeta,
    pub stmts: Vec<ExportStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportStmt {
    Set(ExportSetStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportSetStmt {
    pub key: Spanned<String>,
    pub value: ConstValue,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatchBlock {
    pub meta: ItemMeta,
    pub target: Path,
    pub stmts: Vec<PatchStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatchStmt {
    Default(PatchDefaultStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatchDefaultStmt {
    pub path: Path,
    pub value: ConstValue,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OptionAttachedConstraints {
    pub attrs: Vec<Attr>,
    pub doc: Vec<Spanned<String>>,
    pub requires: Vec<RequireStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RequireStmt {
    pub meta: ItemMeta,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstraintBlock {
    pub meta: ItemMeta,
    pub items: Vec<ConstraintItem>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintItem {
    Require(RequireStmt),
    Attr(Attr),
    Doc(Spanned<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenBlock {
    pub meta: ItemMeta,
    pub condition: Expr,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchBlock {
    pub meta: ItemMeta,
    pub expr: Expr,
    pub cases: Vec<MatchCase>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub meta: ItemMeta,
    pub pattern: MatchPat,
    pub guard: Option<Expr>,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPat {
    Wildcard(Span),
    Paths(Vec<Path>, Span),
}

impl MatchPat {
    pub fn span(&self) -> Span {
        match self {
            MatchPat::Wildcard(span) | MatchPat::Paths(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attr {
    pub kind: AttrKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttrKind {
    Range(IntRange),
    Unit(String),
    Msg(String),
    Secret,
    Cfg(Expr),
    Other { name: String, args: Vec<AttrArg> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttrArg {
    Expr(Expr),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntRange {
    pub start: i128,
    pub end: i128,
    pub inclusive: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool(Span),
    U8(Span),
    U16(Span),
    U32(Span),
    I32(Span),
    String(Span),
    Named(Path),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Bool(span)
            | Type::U8(span)
            | Type::U16(span)
            | Type::U32(span)
            | Type::I32(span)
            | Type::String(span) => *span,
            Type::Named(path) => path.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub segments: Vec<Spanned<String>>,
    pub span: Span,
}

impl Path {
    pub fn to_string(&self) -> String {
        self.segments
            .iter()
            .map(|segment| segment.value.clone())
            .collect::<Vec<_>>()
            .join("::")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Bool(bool, Span),
    Int(i128, Span),
    String(String, Span),
    EnumPath(Path),
}

impl ConstValue {
    pub fn span(&self) -> Span {
        match self {
            ConstValue::Bool(_, span) | ConstValue::Int(_, span) | ConstValue::String(_, span) => {
                *span
            }
            ConstValue::EnumPath(path) => path.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool, Span),
    Int(i128, Span),
    String(String, Span),
    SelfValue(Span),
    Path(Path),
    Call {
        name: Spanned<String>,
        args: Vec<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    InRange {
        expr: Box<Expr>,
        range: IntRange,
        span: Span,
    },
    InSet {
        expr: Box<Expr>,
        elems: Vec<InSetElem>,
        span: Span,
    },
    Group {
        expr: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Bool(_, span)
            | Expr::Int(_, span)
            | Expr::String(_, span)
            | Expr::SelfValue(span) => *span,
            Expr::Path(path) => path.span,
            Expr::Call { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::InRange { span, .. }
            | Expr::InSet { span, .. }
            | Expr::Group { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InSetElem {
    Int(i128, Span),
    Path(Path),
}

impl InSetElem {
    pub fn span(&self) -> Span {
        match self {
            InSetElem::Int(_, span) => *span,
            InSetElem::Path(path) => path.span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValuesFile {
    pub stmts: Vec<ValuesStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValuesStmt {
    Include(IncludeStmt),
    Use(UseStmt),
    Assign(AssignStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncludeStmt {
    pub path: Spanned<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub path: Path,
    pub value: ValueExpr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    Bool(bool, Span),
    Int(i128, Span),
    String(String, Span),
    Path(Path),
    Env {
        name: Spanned<String>,
        fallback: Option<Spanned<String>>,
        span: Span,
    },
}

impl ValueExpr {
    pub fn span(&self) -> Span {
        match self {
            ValueExpr::Bool(_, span) | ValueExpr::Int(_, span) | ValueExpr::String(_, span) => {
                *span
            }
            ValueExpr::Path(path) => path.span,
            ValueExpr::Env { span, .. } => *span,
        }
    }
}
