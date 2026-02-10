use super::*;

pub(super) struct IncludeExpander {
    pub(super) diagnostics: Vec<Diagnostic>,
    pub(super) stack: Vec<PathBuf>,
    root_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub(super) struct ExpandedValuesStmt {
    pub(super) stmt: ValuesStmt,
    pub(super) origin: ValuesStmtOrigin,
}

impl IncludeExpander {
    pub(super) fn with_root(root_dir: PathBuf) -> Self {
        Self {
            diagnostics: Vec::new(),
            stack: Vec::new(),
            root_dir,
        }
    }

    fn resolve_include_target(&self, base_dir: &FsPath, include_path: &str) -> PathBuf {
        if include_path == "@root" {
            return self.root_dir.clone();
        }

        if let Some(stripped) = include_path.strip_prefix("@root/") {
            return self.root_dir.join(stripped);
        }

        base_dir.join(include_path)
    }

    pub(super) fn current_chain(&self, next: &PathBuf) -> Vec<String> {
        self.stack
            .iter()
            .chain(std::iter::once(next))
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
    }

    pub(super) fn expand_file(&mut self, file: &FsPath, output: &mut Vec<ExpandedValuesStmt>) {
        let canonical = match fs::canonicalize(file) {
            Ok(path) => path,
            Err(_) => {
                self.diagnostics.push(
                    Diagnostic::error(
                        "E_INCLUDE_NOT_FOUND",
                        format!("include file not found: {}", file.display()),
                        Span::default(),
                    )
                    .with_source(file.display().to_string())
                    .with_include_chain(self.current_chain(&file.to_path_buf()))
                    .with_note(
                        "fix-it: verify the include path, or use `@root/...` to include from project root",
                    ),
                );
                return;
            }
        };

        if let Some(index) = self.stack.iter().position(|path| path == &canonical) {
            let chain = self.stack[index..]
                .iter()
                .chain(std::iter::once(&canonical))
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(" -> ");
            self.diagnostics.push(
                Diagnostic::error(
                    "E_INCLUDE_CYCLE",
                    format!("include cycle detected: {}", chain),
                    Span::default(),
                )
                .with_source(canonical.display().to_string())
                .with_include_chain(self.current_chain(&canonical)),
            );
            return;
        }

        let text = match fs::read_to_string(&canonical) {
            Ok(content) => content,
            Err(_) => {
                self.diagnostics.push(
                    Diagnostic::error(
                        "E_INCLUDE_NOT_FOUND",
                        format!("failed to read include file: {}", canonical.display()),
                        Span::default(),
                    )
                    .with_source(canonical.display().to_string())
                    .with_include_chain(self.current_chain(&canonical))
                    .with_note(
                        "fix-it: check file permissions, or ensure the include target is readable",
                    ),
                );
                return;
            }
        };

        let (values, mut parse_diags) = parse_values_with_diagnostics(&text);
        for diag in &mut parse_diags {
            if diag.source.is_none() {
                diag.source = Some(canonical.display().to_string());
            }
            if diag.include_chain.is_empty() {
                diag.include_chain = self.current_chain(&canonical);
            }
        }
        self.diagnostics.append(&mut parse_diags);

        self.stack.push(canonical.clone());
        let base_dir = canonical
            .parent()
            .map(FsPath::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));

        for stmt in values.stmts {
            match stmt {
                ValuesStmt::Include(include) => {
                    let target = self.resolve_include_target(&base_dir, &include.path.value);
                    self.expand_file(&target, output);
                }
                other => output.push(ExpandedValuesStmt {
                    stmt: other,
                    origin: ValuesStmtOrigin {
                        source: canonical.display().to_string(),
                        include_chain: self.current_chain(&canonical),
                    },
                }),
            }
        }

        self.stack.pop();
    }
}

pub(super) struct ValuesChecker<'a> {
    symbols: &'a SymbolTable,
    pub(super) diagnostics: Vec<Diagnostic>,
    pub(super) diagnostic_stmt_indexes: Vec<Option<usize>>,
    aliases: HashMap<String, String>,
    assigned: BTreeMap<String, Span>,
    assignments: BTreeMap<String, ResolvedAssignment>,
    resolved: Option<ResolvedConfig>,
    current_stmt_index: Option<usize>,
}

#[derive(Debug, Clone)]
pub(super) struct ResolvedAssignment {
    pub(super) value: ResolvedValue,
    pub(super) source: ValueSource,
    pub(super) span: Span,
    pub(super) stmt_index: Option<usize>,
}

#[derive(Debug, Clone)]
pub(super) struct ParsedValue {
    actual_type: ValueType,
    resolved_value: Option<ResolvedValue>,
    int_value: Option<i128>,
}

impl<'a> ValuesChecker<'a> {
    pub(super) fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            diagnostics: Vec::new(),
            diagnostic_stmt_indexes: Vec::new(),
            aliases: HashMap::new(),
            assigned: BTreeMap::new(),
            assignments: BTreeMap::new(),
            resolved: None,
            current_stmt_index: None,
        }
    }

    pub(super) fn ingest_context(&mut self, context: &HashMap<String, ResolvedValue>) {
        for (path, value) in context {
            self.assignments.insert(
                path.clone(),
                ResolvedAssignment {
                    value: value.clone(),
                    source: ValueSource::Context,
                    span: self.symbols.option_span(path).unwrap_or_default(),
                    stmt_index: None,
                },
            );
        }
    }

    pub(super) fn resolved_config(&self) -> Option<ResolvedConfig> {
        self.resolved.clone()
    }

    pub(super) fn check(&mut self, values: &ValuesFile) {
        for (index, stmt) in values.stmts.iter().enumerate() {
            self.current_stmt_index = Some(index);
            match stmt {
                ValuesStmt::Include(_) => {}
                ValuesStmt::Use(use_stmt) => {
                    self.register_use_alias(use_stmt);
                }
                ValuesStmt::Assign(assign) => {
                    let lhs = self.resolve_assignment_target(&assign.path);
                    if let Some(resolved) = &lhs {
                        self.record_duplicate_assignment(resolved, assign.path.span);
                    }
                    self.check_assignment_value(lhs.as_deref(), &assign.value);
                }
            }
        }
        self.current_stmt_index = None;
    }

    pub(super) fn record_duplicate_assignment(&mut self, target: &str, span: Span) {
        if let Some(previous) = self.assigned.insert(target.to_string(), span) {
            self.push_diag(
                Diagnostic::warning(
                    "W_DUPLICATE_ASSIGNMENT",
                    format!("option `{}` is assigned multiple times; last wins", target),
                    span.join(previous),
                )
                .with_note("fix-it: remove earlier assignment and keep only the final value"),
            );
        }
    }

    pub(super) fn register_use_alias(&mut self, use_stmt: &crate::ast::UseStmt) {
        let Some(alias) = &use_stmt.alias else {
            return;
        };

        if self.aliases.contains_key(&alias.value) {
            self.push_diag(Diagnostic::error(
                "E_USE_ALIAS_CONFLICT",
                format!("use alias `{}` is already defined", alias.value),
                alias.span,
            ));
            return;
        }

        self.aliases
            .insert(alias.value.clone(), use_stmt.path.to_string());
    }

    pub(super) fn resolve_assignment_target(&mut self, path: &Path) -> Option<String> {
        let expanded = expand_with_aliases(path, &self.aliases);
        let candidates = self.symbols.resolve_option_paths(&expanded);

        if candidates.is_empty() {
            self.push_diag(Diagnostic::error(
                "E_SYMBOL_NOT_FOUND",
                format!(
                    "assignment target `{}` cannot be resolved",
                    path.to_string()
                ),
                path.span,
            ));
            return None;
        }

        if candidates.len() > 1 {
            self.push_diag(Diagnostic::error(
                "E_AMBIGUOUS_PATH",
                format!(
                    "assignment target `{}` is ambiguous: {}",
                    path.to_string(),
                    candidates.join(", ")
                ),
                path.span,
            ));
            return None;
        }

        let resolved = candidates[0].clone();
        if resolved == "ctx" || resolved.starts_with("ctx::") {
            self.push_diag(
                Diagnostic::error(
                    "E_CONTEXT_ASSIGNMENT_NOT_ALLOWED",
                    "assigning values to `ctx` is not allowed",
                    path.span,
                )
                .with_note("fix-it: remove this assignment and pass context values via `--context`"),
            );
            return None;
        }

        Some(resolved)
    }

    pub(super) fn check_assignment_value(&mut self, target: Option<&str>, value: &ValueExpr) {
        let Some(target) = target else {
            return;
        };

        let Some(expected) = self.symbols.option_type(target).cloned() else {
            self.push_diag(
                Diagnostic::error(
                    "E_SYMBOL_NOT_FOUND",
                    format!("assignment target `{}` is not an option", target),
                    value.span(),
                )
                .with_path(target),
            );
            return;
        };

        let parsed = self.parse_value_expr(value, &expected);
        let actual = parsed.actual_type;
        if !actual.is_unknown() && !expected.same_as(&actual) {
            self.push_diag(
                Diagnostic::error(
                    "E_TYPE_MISMATCH",
                    format!(
                        "value type mismatch for `{}`: expected {:?}, got {:?}",
                        target, expected, actual
                    ),
                    value.span(),
                )
                .with_path(target),
            );
            return;
        }

        if expected.is_int() {
            if let Some(actual) = parsed.int_value {
                if let Some((min, max)) = int_value_type_bounds(&expected) {
                    if actual < min || actual > max {
                        self.push_diag(
                            Diagnostic::error(
                                "E_TYPE_MISMATCH",
                                format!(
                                    "value `{}` for `{}` is out of type bounds [{}..={}]",
                                    actual, target, min, max
                                ),
                                value.span(),
                            )
                            .with_path(target),
                        );
                        return;
                    }
                }
            }

            if let (Some(range), Some(actual)) =
                (self.symbols.option_range(target), parsed.int_value)
            {
                let violates = if range.inclusive {
                    actual < range.start || actual > range.end
                } else {
                    actual < range.start || actual >= range.end
                };

                if violates {
                    let max = if range.inclusive {
                        range.end.to_string()
                    } else {
                        format!("{} (exclusive)", range.end)
                    };
                    let is_secret = self.symbols.option_is_secret(target);
                    self.push_diag(
                        Diagnostic::error(
                            "E_RANGE_VIOLATION",
                            format!(
                                "value `{}` for `{}` is outside range [{}..={}]",
                                actual, target, range.start, max
                            ),
                            value.span(),
                        )
                        .with_path(target)
                        .with_arg("actual", redacted_int_arg(actual, is_secret))
                        .with_arg("min", DiagnosticArgValue::Int(range.start))
                        .with_arg("max", DiagnosticArgValue::Int(range.end))
                        .with_arg("inclusive", DiagnosticArgValue::Bool(range.inclusive)),
                    );
                    return;
                }
            }
        }

        if let Some(resolved_value) = parsed.resolved_value {
            self.assignments.insert(
                target.to_string(),
                ResolvedAssignment {
                    value: resolved_value,
                    source: ValueSource::User,
                    span: value.span(),
                    stmt_index: self.current_stmt_index,
                },
            );
        }
    }

    pub(super) fn parse_value_expr(
        &mut self,
        value: &ValueExpr,
        expected: &ValueType,
    ) -> ParsedValue {
        match value {
            ValueExpr::Bool(raw, _) => ParsedValue {
                actual_type: ValueType::Bool,
                resolved_value: Some(ResolvedValue::Bool(*raw)),
                int_value: None,
            },
            ValueExpr::Int(raw, _) => ParsedValue {
                actual_type: ValueType::UntypedInt,
                resolved_value: Some(ResolvedValue::Int(*raw)),
                int_value: Some(*raw),
            },
            ValueExpr::String(raw, _) => ParsedValue {
                actual_type: ValueType::String,
                resolved_value: Some(ResolvedValue::String(raw.clone())),
                int_value: None,
            },
            ValueExpr::Path(path) => {
                let actual_type = self.value_expr_type(value, expected);
                let resolved_value = {
                    let expanded = expand_with_aliases(path, &self.aliases);
                    let mut matches = self.symbols.resolve_enum_variant_paths(&expanded);
                    if matches.len() == 1 {
                        Some(ResolvedValue::EnumVariant(matches.remove(0)))
                    } else {
                        None
                    }
                };
                ParsedValue {
                    actual_type,
                    int_value: resolved_value.as_ref().and_then(ResolvedValue::as_int),
                    resolved_value,
                }
            }
            ValueExpr::Env { name, fallback, .. } => self.parse_env_value(
                name.value.as_str(),
                fallback.as_ref().map(|value| value.value.as_str()),
                expected,
                value.span(),
            ),
        }
    }

    pub(super) fn parse_env_value(
        &mut self,
        name: &str,
        fallback: Option<&str>,
        expected: &ValueType,
        span: Span,
    ) -> ParsedValue {
        let raw = match std::env::var(name) {
            Ok(value) => value,
            Err(_) => {
                if let Some(fallback) = fallback {
                    return self.parse_env_raw_value(name, fallback.to_string(), expected, span);
                }

                self.push_diag(Diagnostic::error(
                    "E_ENV_NOT_SET",
                    format!("environment variable `{}` is not set", name),
                    span,
                ));
                return ParsedValue {
                    actual_type: ValueType::Unknown,
                    resolved_value: None,
                    int_value: None,
                };
            }
        };

        self.parse_env_raw_value(name, raw, expected, span)
    }

    fn parse_env_raw_value(
        &mut self,
        name: &str,
        raw: String,
        expected: &ValueType,
        span: Span,
    ) -> ParsedValue {
        let raw_ref = raw.as_str();

        let (actual_type, resolved_value) = match expected {
            ValueType::Bool => {
                if raw_ref == "true" {
                    (ValueType::Bool, Some(ResolvedValue::Bool(true)))
                } else if raw_ref == "false" {
                    (ValueType::Bool, Some(ResolvedValue::Bool(false)))
                } else {
                    self.push_env_parse_failed(name, raw_ref, "bool", span);
                    (ValueType::Unknown, None)
                }
            }
            ValueType::Int(int_ty) => match parse_env_int(raw_ref) {
                Some(value) => {
                    let (min, max) = int_bounds_for_int_type(*int_ty);
                    if value < min || value > max {
                        self.push_env_parse_failed(
                            name,
                            raw_ref,
                            &format!("{} in range [{}..={}]", int_type_name(*int_ty), min, max),
                            span,
                        );
                        (ValueType::Unknown, None)
                    } else {
                        (ValueType::Int(*int_ty), Some(ResolvedValue::Int(value)))
                    }
                }
                None => {
                    self.push_env_parse_failed(name, raw_ref, "int", span);
                    (ValueType::Unknown, None)
                }
            },
            ValueType::UntypedInt => match parse_env_int(raw_ref) {
                Some(value) => (ValueType::UntypedInt, Some(ResolvedValue::Int(value))),
                None => {
                    self.push_env_parse_failed(name, raw_ref, "int", span);
                    (ValueType::Unknown, None)
                }
            },
            ValueType::String => (ValueType::String, Some(ResolvedValue::String(raw))),
            ValueType::Enum(expected_enum) => {
                let enum_variant =
                    self.resolve_env_enum_variant(name, raw_ref, expected_enum, span);
                if let Some(variant) = enum_variant {
                    (
                        ValueType::Enum(expected_enum.clone()),
                        Some(ResolvedValue::EnumVariant(variant)),
                    )
                } else {
                    (ValueType::Unknown, None)
                }
            }
            ValueType::Unknown => (ValueType::Unknown, None),
        };

        let int_value = resolved_value.as_ref().and_then(ResolvedValue::as_int);
        ParsedValue {
            actual_type,
            resolved_value,
            int_value,
        }
    }

    pub(super) fn resolve_env_enum_variant(
        &mut self,
        env_name: &str,
        raw: &str,
        expected_enum: &str,
        span: Span,
    ) -> Option<String> {
        let mut candidates = self
            .symbols
            .resolve_enum_variant_paths(raw)
            .into_iter()
            .filter(|candidate| {
                self.symbols
                    .enum_owner_of_variant(candidate)
                    .is_some_and(|owner| enum_name_matches(expected_enum, owner))
            })
            .collect::<Vec<_>>();
        candidates.sort();
        candidates.dedup();

        if candidates.is_empty() {
            self.push_env_parse_failed(env_name, raw, &format!("enum {}", expected_enum), span);
            return None;
        }

        if candidates.len() > 1 {
            self.push_diag(Diagnostic::error(
                "E_AMBIGUOUS_ENUM_VARIANT",
                format!(
                    "env value `{}` is ambiguous for enum `{}`: {}",
                    raw,
                    expected_enum,
                    candidates.join(", ")
                ),
                span,
            ));
            return None;
        }

        candidates.into_iter().next()
    }

    pub(super) fn value_expr_type(&mut self, value: &ValueExpr, expected: &ValueType) -> ValueType {
        match value {
            ValueExpr::Bool(_, _) => ValueType::Bool,
            ValueExpr::Int(_, _) => ValueType::UntypedInt,
            ValueExpr::String(_, _) => ValueType::String,
            ValueExpr::Env { name, fallback, .. } => self.env_value_type(
                name.value.as_str(),
                fallback.as_ref().map(|value| value.value.as_str()),
                expected,
                value.span(),
            ),
            ValueExpr::Path(path) => {
                let expanded = expand_with_aliases(path, &self.aliases);
                let option_matches = self.symbols.resolve_option_paths(&expanded);
                if !option_matches.is_empty() {
                    let resolved = option_matches[0].clone();
                    self.push_diag(
                        Diagnostic::error(
                            "E_VALUE_PATH_RESOLVES_TO_OPTION",
                            format!("value path `{}` resolves to option", path.to_string()),
                            path.span,
                        )
                        .with_path(resolved),
                    );
                    return ValueType::Unknown;
                }

                let mut matches = self.symbols.resolve_enum_variant_paths(&expanded);
                if matches.is_empty() {
                    self.push_diag(Diagnostic::error(
                        "E_VALUE_PATH_NOT_ENUM_VARIANT",
                        format!("value path `{}` is not an enum variant", path.to_string()),
                        path.span,
                    ));
                    return ValueType::Unknown;
                }

                if matches.len() > 1 {
                    self.push_diag(Diagnostic::error(
                        "E_AMBIGUOUS_ENUM_VARIANT",
                        format!(
                            "enum variant `{}` is ambiguous: {}",
                            path.to_string(),
                            matches.join(", ")
                        ),
                        path.span,
                    ));
                    return ValueType::Unknown;
                }

                let resolved = matches.remove(0);
                let enum_name = self
                    .symbols
                    .enum_owner_of_variant(&resolved)
                    .map(|owner| owner.rsplit("::").next().unwrap_or(owner).to_string())
                    .unwrap_or_default();
                ValueType::Enum(enum_name)
            }
        }
    }

    pub(super) fn env_value_type(
        &mut self,
        name: &str,
        fallback: Option<&str>,
        expected: &ValueType,
        span: Span,
    ) -> ValueType {
        self.parse_env_value(name, fallback, expected, span)
            .actual_type
    }

    pub(super) fn push_env_parse_failed(
        &mut self,
        name: &str,
        value: &str,
        expected: &str,
        span: Span,
    ) {
        self.push_diag(Diagnostic::error(
            "E_ENV_PARSE_FAILED",
            format!(
                "failed to parse env `{}` value `{}` as {}",
                name, value, expected
            ),
            span,
        ));
    }

    pub(super) fn post_check_diagnostics(&mut self) -> Vec<(Diagnostic, Option<usize>)> {
        let runtime = self.runtime_state();
        self.resolved = Some(build_resolved_config(self.symbols, &runtime));

        let mut diagnostics = Vec::new();
        diagnostics.extend(self.runtime_inactive_assignment_diagnostics(&runtime));
        diagnostics.extend(self.missing_value_diagnostics(&runtime));
        diagnostics.extend(self.missing_context_diagnostics(&runtime));
        diagnostics.extend(self.runtime_require_diagnostics(&runtime));
        diagnostics
    }

    pub(super) fn runtime_state(&self) -> RuntimeState {
        evaluate_runtime_state(self.symbols, &self.assignments)
    }

    pub(super) fn runtime_inactive_assignment_diagnostics(
        &self,
        runtime: &RuntimeState,
    ) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        for (path, assignment) in &self.assignments {
            if assignment.source != ValueSource::User {
                continue;
            }
            if runtime.is_active(path) {
                continue;
            }

            diagnostics.push((
                Diagnostic::warning(
                    "W_INACTIVE_ASSIGNMENT",
                    format!(
                        "assignment to inactive option `{}` is ignored by final activation",
                        path
                    ),
                    assignment.span,
                )
                .with_path(path.clone()),
                assignment.stmt_index,
            ));
        }
        diagnostics
    }

    pub(super) fn missing_value_diagnostics(
        &self,
        runtime: &RuntimeState,
    ) -> Vec<(Diagnostic, Option<usize>)> {
        let mut option_paths = self
            .symbols
            .option_types
            .keys()
            .map(|path| path.as_str().to_string())
            .collect::<Vec<_>>();
        option_paths.sort();

        let mut diagnostics = Vec::new();
        for option_path in option_paths {
            if option_path == "ctx" || option_path.starts_with("ctx::") {
                continue;
            }
            if !runtime.is_active(&option_path) {
                continue;
            }
            if runtime.value_of(&option_path).is_some() {
                continue;
            }

            diagnostics.push((
                Diagnostic::error(
                    "E_MISSING_VALUE",
                    format!(
                        "active option `{}` has no value from assignments or defaults",
                        option_path
                    ),
                    self.symbols.option_span(&option_path).unwrap_or_default(),
                )
                .with_path(option_path),
                None,
            ));
        }

        diagnostics
    }

    pub(super) fn missing_context_diagnostics(
        &self,
        runtime: &RuntimeState,
    ) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        for ctx_path in &runtime.ctx_references {
            if runtime.value_of(ctx_path).is_some() {
                continue;
            }

            diagnostics.push((
                Diagnostic::error(
                    "E_MISSING_CONTEXT_VALUE",
                    format!(
                        "context option `{}` requires an injected context value",
                        ctx_path
                    ),
                    self.symbols.option_span(ctx_path).unwrap_or_default(),
                )
                .with_path(ctx_path.clone()),
                None,
            ));
        }
        diagnostics
    }

    pub(super) fn runtime_require_diagnostics(
        &self,
        runtime: &RuntimeState,
    ) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        let mut scope = Vec::new();
        let mut aliases = HashMap::new();
        let mut require_counters = HashMap::new();
        collect_runtime_require_diagnostics(
            self.symbols,
            self.symbols.schema_items(),
            &mut scope,
            &mut aliases,
            runtime,
            &mut diagnostics,
            &mut require_counters,
        );
        diagnostics
    }

    pub(super) fn push_diag(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
        self.diagnostic_stmt_indexes.push(self.current_stmt_index);
    }
}
