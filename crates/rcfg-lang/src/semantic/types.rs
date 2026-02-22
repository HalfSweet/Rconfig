use super::*;
use std::borrow::Borrow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Mod,
    Enum,
    Option,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolInfo {
    pub kind: SymbolKind,
    pub path: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolOccurrenceRole {
    Definition,
    Reference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolOccurrence {
    pub path: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub role: SymbolOccurrenceRole,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolPositionIndex {
    occurrences: Vec<SymbolOccurrence>,
}

impl SymbolPositionIndex {
    pub fn new(mut occurrences: Vec<SymbolOccurrence>) -> Self {
        occurrences.sort_by(|left, right| {
            (left.span.start, left.span.end, left.path.as_str()).cmp(&(
                right.span.start,
                right.span.end,
                right.path.as_str(),
            ))
        });
        Self { occurrences }
    }

    pub fn occurrences(&self) -> &[SymbolOccurrence] {
        &self.occurrences
    }

    pub fn find_symbol_at_offset(&self, offset: usize) -> Option<&SymbolOccurrence> {
        let boundary = self
            .occurrences
            .partition_point(|occurrence| occurrence.span.start <= offset);

        let mut best_index = None;
        for index in (0..boundary).rev() {
            let occurrence = &self.occurrences[index];
            let contains = if occurrence.span.start == occurrence.span.end {
                offset == occurrence.span.start
            } else {
                offset >= occurrence.span.start && offset < occurrence.span.end
            };
            if !contains {
                continue;
            }

            match best_index {
                None => best_index = Some(index),
                Some(current_index) => {
                    let current = &self.occurrences[current_index];
                    let current_width = current.span.end.saturating_sub(current.span.start);
                    let next_width = occurrence.span.end.saturating_sub(occurrence.span.start);
                    if next_width < current_width
                        || (next_width == current_width
                            && current.role == SymbolOccurrenceRole::Definition
                            && occurrence.role == SymbolOccurrenceRole::Reference)
                    {
                        best_index = Some(index);
                    }
                }
            }
        }

        best_index.map(|index| &self.occurrences[index])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct SymbolPath(String);

impl SymbolPath {
    pub(super) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for SymbolPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl Borrow<str> for SymbolPath {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct VariantPath(String);

impl VariantPath {
    pub(super) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for VariantPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl Borrow<str> for VariantPath {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct EnumPath(String);

impl EnumPath {
    pub(super) fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub(super) fn into_inner(self) -> String {
        self.0
    }
}

impl From<String> for EnumPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct OptionPath(String);

impl OptionPath {
    pub(super) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for OptionPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl Borrow<str> for OptionPath {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Bool,
    Int(IntType),
    UntypedInt,
    String,
    Enum(String),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    U8,
    U16,
    U32,
    I32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedValue {
    Bool(bool),
    Int(i128),
    String(String),
    EnumVariant(String),
}

impl ResolvedValue {
    pub(super) fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub(super) fn as_int(&self) -> Option<i128> {
        if let Self::Int(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    pub(super) fn as_string(&self) -> Option<&str> {
        if let Self::String(value) = self {
            Some(value)
        } else {
            None
        }
    }

    pub(super) fn as_enum_variant(&self) -> Option<&str> {
        if let Self::EnumVariant(value) = self {
            Some(value)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueSource {
    User,
    Patch,
    Default,
    Context,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedOption {
    pub path: String,
    pub active: bool,
    pub value_type: Option<ValueType>,
    pub value: Option<ResolvedValue>,
    pub source: Option<ValueSource>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ResolvedConfig {
    pub options: Vec<ResolvedOption>,
}

#[derive(Debug, Clone)]
pub(super) struct RuntimeState {
    pub(super) active: BTreeSet<String>,
    pub(super) values: BTreeMap<String, ResolvedValue>,
    pub(super) sources: BTreeMap<String, ValueSource>,
    pub(super) ctx_references: BTreeSet<String>,
}

impl RuntimeState {
    pub(super) fn is_active(&self, path: &str) -> bool {
        self.active.contains(path)
    }

    pub(super) fn value_of(&self, path: &str) -> Option<&ResolvedValue> {
        self.values.get(path)
    }
}

impl ValueType {
    pub(super) fn is_bool(&self) -> bool {
        matches!(self, ValueType::Bool)
    }

    pub(super) fn is_int(&self) -> bool {
        matches!(self, ValueType::Int(_) | ValueType::UntypedInt)
    }

    pub(super) fn concrete_int(&self) -> Option<IntType> {
        if let ValueType::Int(ty) = self {
            Some(*ty)
        } else {
            None
        }
    }

    pub(super) fn is_string(&self) -> bool {
        matches!(self, ValueType::String)
    }

    pub(super) fn is_unknown(&self) -> bool {
        matches!(self, ValueType::Unknown)
    }

    pub(super) fn same_as(&self, other: &ValueType) -> bool {
        match (self, other) {
            (ValueType::Bool, ValueType::Bool)
            | (ValueType::String, ValueType::String)
            | (ValueType::Unknown, ValueType::Unknown) => true,
            (ValueType::UntypedInt, ValueType::UntypedInt) => true,
            (ValueType::UntypedInt, ValueType::Int(_))
            | (ValueType::Int(_), ValueType::UntypedInt) => true,
            (ValueType::Int(left), ValueType::Int(right)) => left == right,
            (ValueType::Enum(left), ValueType::Enum(right)) => left == right,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub(super) symbols: BTreeMap<SymbolPath, SymbolInfo>,
    pub(super) option_types: BTreeMap<OptionPath, ValueType>,
    pub(super) option_defaults: BTreeMap<OptionPath, ConstValue>,
    pub(super) option_ranges: BTreeMap<OptionPath, IntRange>,
    pub(super) option_spans: BTreeMap<OptionPath, Span>,
    pub(super) symbol_spans: BTreeMap<SymbolPath, Span>,
    pub(super) symbol_docs: BTreeMap<SymbolPath, Vec<Spanned<String>>>,
    pub(super) option_secrets: BTreeMap<OptionPath, bool>,
    pub(super) option_always_active: BTreeMap<OptionPath, bool>,
    pub(super) enum_variants: BTreeMap<VariantPath, EnumPath>,
    pub(super) enum_variant_spans: BTreeMap<VariantPath, Span>,
    pub(super) enum_variant_docs: BTreeMap<VariantPath, Vec<Spanned<String>>>,
    pub(super) symbol_references: BTreeMap<String, Vec<Span>>,
    pub(super) schema_items: Vec<Item>,
}

impl SymbolTable {
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn get(&self, path: &str) -> Option<&SymbolInfo> {
        self.symbols.get(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &SymbolInfo)> {
        self.symbols
            .iter()
            .map(|(path, info)| (path.as_str(), info))
    }

    pub fn option_type(&self, path: &str) -> Option<&ValueType> {
        self.option_types.get(path)
    }

    pub fn option_default(&self, path: &str) -> Option<&ConstValue> {
        self.option_defaults.get(path)
    }

    pub fn schema_items(&self) -> &[Item] {
        &self.schema_items
    }

    pub(super) fn insert_symbol(&mut self, symbol: SymbolInfo) {
        self.symbols
            .insert(SymbolPath::from(symbol.path.clone()), symbol);
    }

    pub(super) fn insert_option_type(&mut self, path: String, ty: ValueType) {
        self.option_types.insert(OptionPath::from(path), ty);
    }

    pub(super) fn insert_option_default(&mut self, path: String, value: ConstValue) {
        self.option_defaults.insert(OptionPath::from(path), value);
    }

    pub(super) fn insert_option_range(&mut self, path: String, range: IntRange) {
        self.option_ranges.insert(OptionPath::from(path), range);
    }

    pub(super) fn insert_option_span(&mut self, path: String, span: Span) {
        self.option_spans.insert(OptionPath::from(path), span);
    }

    pub(super) fn insert_symbol_span(&mut self, path: String, span: Span) {
        self.symbol_spans.insert(SymbolPath::from(path), span);
    }

    pub(super) fn insert_symbol_docs(&mut self, path: String, docs: Vec<Spanned<String>>) {
        if docs.is_empty() {
            return;
        }
        self.symbol_docs.insert(SymbolPath::from(path), docs);
    }

    pub(super) fn insert_symbol_reference(&mut self, path: String, span: Span) {
        let entry = self.symbol_references.entry(path).or_default();
        if !entry.contains(&span) {
            entry.push(span);
            entry.sort_by_key(|item| (item.start, item.end));
        }
    }

    pub(super) fn insert_option_secret(&mut self, path: String, secret: bool) {
        self.option_secrets.insert(OptionPath::from(path), secret);
    }

    pub(super) fn insert_option_always_active(&mut self, path: String, is_always_active: bool) {
        self.option_always_active
            .insert(OptionPath::from(path), is_always_active);
    }

    pub(super) fn set_schema_items(&mut self, items: Vec<Item>) {
        self.schema_items = items;
    }

    pub fn option_range(&self, path: &str) -> Option<&IntRange> {
        self.option_ranges.get(path)
    }

    pub fn option_span(&self, path: &str) -> Option<Span> {
        self.option_spans.get(path).copied()
    }

    pub fn enum_variant_span(&self, variant_path: &str) -> Option<Span> {
        self.enum_variant_spans.get(variant_path).copied()
    }

    pub fn symbol_span(&self, path: &str) -> Option<Span> {
        self.symbol_spans.get(path).copied()
    }

    pub fn symbol_docs(&self, path: &str) -> Option<&[Spanned<String>]> {
        self.symbol_docs.get(path).map(Vec::as_slice)
    }

    pub fn symbol_references(&self, path: &str) -> Option<&[Span]> {
        self.symbol_references.get(path).map(Vec::as_slice)
    }

    pub fn build_position_index(&self) -> SymbolPositionIndex {
        let mut occurrences = Vec::new();

        for (path, info) in self.iter() {
            if let Some(span) = self.symbol_span(path) {
                occurrences.push(SymbolOccurrence {
                    path: path.to_string(),
                    kind: info.kind,
                    span,
                    role: SymbolOccurrenceRole::Definition,
                });
            }
        }

        for (variant_path, span) in &self.enum_variant_spans {
            occurrences.push(SymbolOccurrence {
                path: variant_path.as_str().to_string(),
                kind: SymbolKind::Enum,
                span: *span,
                role: SymbolOccurrenceRole::Definition,
            });
        }

        for (path, spans) in &self.symbol_references {
            let Some(kind) = self.symbol_kind_for_path(path) else {
                continue;
            };
            for span in spans {
                occurrences.push(SymbolOccurrence {
                    path: path.clone(),
                    kind,
                    span: *span,
                    role: SymbolOccurrenceRole::Reference,
                });
            }
        }

        SymbolPositionIndex::new(occurrences)
    }

    pub fn option_is_secret(&self, path: &str) -> bool {
        self.option_secrets.get(path).copied().unwrap_or(false)
    }

    pub(super) fn option_is_always_active(&self, path: &str) -> bool {
        self.option_always_active.get(path).copied().unwrap_or(true)
    }

    pub(super) fn insert_enum_variant(
        &mut self,
        variant_path: String,
        enum_path: String,
        span: Span,
    ) -> Option<String> {
        let variant = VariantPath::from(variant_path);
        self.enum_variant_spans.insert(variant.clone(), span);
        self.enum_variants
            .insert(variant, EnumPath::from(enum_path))
            .map(EnumPath::into_inner)
    }

    pub(super) fn insert_enum_variant_doc(
        &mut self,
        variant_path: String,
        docs: Vec<Spanned<String>>,
    ) {
        if docs.is_empty() {
            return;
        }
        self.enum_variant_docs
            .insert(VariantPath::from(variant_path), docs);
    }

    pub fn enum_variant_docs(&self, variant_path: &str) -> Option<&[Spanned<String>]> {
        self.enum_variant_docs.get(variant_path).map(Vec::as_slice)
    }

    fn symbol_kind_for_path(&self, path: &str) -> Option<SymbolKind> {
        if let Some(info) = self.get(path) {
            return Some(info.kind);
        }

        if self.enum_variants.contains_key(path) {
            return Some(SymbolKind::Enum);
        }

        None
    }

    pub(super) fn resolve_path_type_raw_in_scope(
        &self,
        scope: &[String],
        raw_path: &str,
    ) -> ResolvePathResult {
        let candidates = build_candidate_paths(scope, raw_path);

        let mut matches = Vec::new();
        let mut seen = HashSet::new();

        for candidate in candidates {
            if let Some(ty) = self.option_types.get(candidate.as_str())
                && seen.insert(candidate.clone())
            {
                matches.push((candidate.clone(), ty.clone()));
            }

            if let Some(enum_path) = self.enum_variants.get(candidate.as_str())
                && seen.insert(candidate.clone())
            {
                matches.push((
                    candidate.clone(),
                    ValueType::Enum(enum_path.as_str().to_string()),
                ));
            }
        }

        if matches.is_empty() {
            return ResolvePathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolvePathResult::Ambiguous(
                matches
                    .into_iter()
                    .map(|(path, _)| path)
                    .collect::<Vec<_>>(),
            );
        }

        let (_, ty) = matches.pop().expect("matches length was checked");
        ResolvePathResult::Resolved(ty)
    }

    pub fn enum_variant_names(&self, enum_name: &str) -> Option<Vec<String>> {
        let mut variants = self
            .enum_variants
            .iter()
            .filter(|(_, owner)| enum_name_matches(enum_name, owner.as_str()))
            .filter_map(|(variant_path, _)| {
                variant_path
                    .as_str()
                    .rsplit("::")
                    .next()
                    .map(str::to_string)
            })
            .collect::<Vec<_>>();

        if variants.is_empty() {
            return None;
        }

        variants.sort();
        variants.dedup();
        Some(variants)
    }

    pub fn enum_owner_of_variant(&self, variant_path: &str) -> Option<&str> {
        self.enum_variants.get(variant_path).map(EnumPath::as_str)
    }

    pub(super) fn resolve_option_paths(&self, raw_path: &str) -> Vec<String> {
        self.option_types
            .keys()
            .filter(|candidate| path_matches(candidate.as_str(), raw_path))
            .map(|candidate| candidate.as_str().to_string())
            .collect::<Vec<_>>()
    }

    pub(super) fn resolve_enum_variant_paths(&self, raw_path: &str) -> Vec<String> {
        self.enum_variants
            .keys()
            .filter(|candidate| path_matches(candidate.as_str(), raw_path))
            .map(|candidate| candidate.as_str().to_string())
            .collect::<Vec<_>>()
    }

    pub(super) fn resolve_option_path_raw_in_scope(
        &self,
        scope: &[String],
        raw_path: &str,
    ) -> ResolveOptionPathResult {
        let candidates = build_candidate_paths(scope, raw_path);

        let mut matches = Vec::new();
        for candidate in candidates {
            if let Some(ty) = self.option_types.get(candidate.as_str()) {
                matches.push((candidate.clone(), ty.clone()));
            }
        }

        if matches.is_empty() {
            return ResolveOptionPathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolveOptionPathResult::Ambiguous(
                matches
                    .into_iter()
                    .map(|(path, _)| path)
                    .collect::<Vec<_>>(),
            );
        }

        let (path, ty) = matches.pop().expect("matches length was checked");
        ResolveOptionPathResult::Resolved(path, ty)
    }

    pub(super) fn resolve_enum_variant_path_raw_in_scope(
        &self,
        scope: &[String],
        raw_path: &str,
    ) -> ResolveEnumVariantPathResult {
        let candidates = build_candidate_paths(scope, raw_path);

        let mut matches = Vec::new();
        for candidate in candidates {
            if let Some(enum_path) = self.enum_variants.get(candidate.as_str()) {
                matches.push((candidate.clone(), enum_path.as_str().to_string()));
            }
        }

        if matches.is_empty() {
            return ResolveEnumVariantPathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolveEnumVariantPathResult::Ambiguous(
                matches
                    .into_iter()
                    .map(|(path, _)| path)
                    .collect::<Vec<_>>(),
            );
        }

        let (variant_path, enum_path) = matches.pop().expect("matches length was checked");
        ResolveEnumVariantPathResult::Resolved(variant_path, enum_path)
    }

    pub(super) fn path_resolves_to_option_in_scope(&self, scope: &[String], path: &Path) -> bool {
        self.path_resolves_to_option_raw_in_scope(scope, &path.to_string())
    }

    pub(super) fn path_resolves_to_option_raw_in_scope(
        &self,
        scope: &[String],
        raw_path: &str,
    ) -> bool {
        let candidates = build_candidate_paths(scope, raw_path);
        candidates
            .iter()
            .any(|candidate| self.option_types.contains_key(candidate.as_str()))
    }
}

#[derive(Debug, Clone)]
pub struct SemanticReport {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuesStmtOrigin {
    pub source: String,
    pub include_chain: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ValuesAnalysisReport {
    pub values: ValuesFile,
    pub resolved: ResolvedConfig,
    pub stmt_origins: Vec<ValuesStmtOrigin>,
    pub diagnostics: Vec<Diagnostic>,
    pub diagnostic_stmt_indexes: Vec<Option<usize>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlannedExport {
    pub path: String,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolFalseExportStyle {
    Omit,
    DefineZero,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumExportStyle {
    OneHot,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntExportFormat {
    Decimal,
    Hex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportNameRule {
    PkgPath,
    PathOnly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportOptions {
    pub include_secrets: bool,
    pub include_context: bool,
    pub c_prefix: String,
    pub cmake_prefix: String,
    pub bool_false_style: BoolFalseExportStyle,
    pub enum_export_style: EnumExportStyle,
    pub int_export_format: IntExportFormat,
    pub export_name_rule: ExportNameRule,
}

impl Default for ExportOptions {
    fn default() -> Self {
        Self {
            include_secrets: false,
            include_context: false,
            c_prefix: "CONFIG_".to_string(),
            cmake_prefix: "CFG_".to_string(),
            bool_false_style: BoolFalseExportStyle::Omit,
            enum_export_style: EnumExportStyle::OneHot,
            int_export_format: IntExportFormat::Decimal,
            export_name_rule: ExportNameRule::PkgPath,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedExports {
    pub c_header: String,
    pub cmake: String,
    pub diagnostics: Vec<Diagnostic>,
}
