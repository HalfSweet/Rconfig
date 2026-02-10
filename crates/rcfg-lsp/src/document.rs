#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentKind {
    Schema,
    Values,
    Unknown,
}
