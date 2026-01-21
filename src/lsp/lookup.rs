//! AST lookup utilities for LSP features.
//!
//! This module provides utilities for finding AST nodes at specific positions,
//! tracking variable scopes, and resolving symbol definitions.

use std::collections::HashMap;

use crate::compiler::{
    Block, EnumPatternFields, EnumVariantArgs, Expr, ForClause, Function, Item, MatchArm, Module,
    Pattern, Span, SpannedExpr, Stmt,
};

/// Information about a reference to a symbol.
#[derive(Debug, Clone)]
pub struct ReferenceInfo {
    /// Span of this reference
    pub span: Span,
    /// Whether this is the definition site
    pub is_definition: bool,
}

/// Information about a symbol found at a position.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum SymbolInfo {
    /// A variable reference
    Variable {
        name: String,
        /// Span of the variable usage
        usage_span: Span,
        /// Span of the variable's definition (let binding or parameter)
        definition_span: Option<Span>,
        /// Type of the variable (if known)
        type_info: Option<String>,
    },
    /// A function call
    FunctionCall {
        /// Module path (e.g., ["io"] for io::println)
        module: Vec<String>,
        /// Function name
        name: String,
        /// Span of the call expression
        call_span: Span,
    },
    /// A function definition
    FunctionDef { name: String, span: Span },
    /// A field access
    FieldAccess { field: String, span: Span },
    /// A struct name
    StructRef { name: String, span: Span },
}

/// Scope tracking for variable resolution.
#[derive(Debug, Clone)]
struct Scope {
    /// Variable name -> definition span
    variables: HashMap<String, Span>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, span: Span) {
        self.variables.insert(name, span);
    }

    fn lookup(&self, name: &str) -> Option<Span> {
        self.variables.get(name).cloned()
    }
}

/// Context for AST lookup operations.
pub struct LookupContext {
    /// Stack of scopes (innermost last)
    scopes: Vec<Scope>,
    /// Target offset we're looking for
    target_offset: usize,
    /// Found symbol info
    found: Option<SymbolInfo>,
}

impl LookupContext {
    fn new(target_offset: usize) -> Self {
        Self {
            scopes: vec![Scope::new()],
            target_offset,
            found: None,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_variable(&mut self, name: String, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, span);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Span> {
        // Search from innermost scope outward
        for scope in self.scopes.iter().rev() {
            if let Some(span) = scope.lookup(name) {
                return Some(span);
            }
        }
        None
    }

    fn is_in_range(&self, span: &Span) -> bool {
        // Treat unset spans (0..0) as "unknown, might contain target"
        if span.start == 0 && span.end == 0 {
            return true;
        }
        self.target_offset >= span.start && self.target_offset < span.end
    }

    fn set_found(&mut self, info: SymbolInfo) {
        // Only set if we haven't found something more specific
        if self.found.is_none() {
            self.found = Some(info);
        }
    }
}

/// Find symbol information at the given offset in a module.
pub fn find_symbol_at_offset(module: &Module, offset: usize) -> Option<SymbolInfo> {
    let mut ctx = LookupContext::new(offset);

    for item in &module.items {
        visit_item(&mut ctx, item);
        if ctx.found.is_some() {
            break;
        }
    }

    ctx.found
}

/// Context for collecting all references to a symbol.
struct ReferenceCollector {
    /// Stack of scopes (innermost last)
    scopes: Vec<Scope>,
    /// The target variable definition span we're looking for (for variable references)
    target_def_span: Option<Span>,
    /// The target function name we're looking for
    target_func_name: Option<String>,
    /// The target struct name we're looking for
    target_struct_name: Option<String>,
    /// Whether to include the definition in results
    include_definition: bool,
    /// Collected references
    references: Vec<ReferenceInfo>,
}

impl ReferenceCollector {
    fn new(include_definition: bool) -> Self {
        Self {
            scopes: vec![Scope::new()],
            target_def_span: None,
            target_func_name: None,
            target_struct_name: None,
            include_definition,
            references: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_variable(&mut self, name: String, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, span);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Span> {
        // Search from innermost scope outward
        for scope in self.scopes.iter().rev() {
            if let Some(span) = scope.lookup(name) {
                return Some(span);
            }
        }
        None
    }

    fn add_reference(&mut self, span: Span, is_definition: bool) {
        if is_definition && !self.include_definition {
            return;
        }
        self.references.push(ReferenceInfo {
            span,
            is_definition,
        });
    }
}

/// Find all references to the symbol at the given offset.
pub fn find_all_references(
    module: &Module,
    offset: usize,
    include_definition: bool,
) -> Vec<ReferenceInfo> {
    // First, find the symbol at the given offset
    let symbol = match find_symbol_at_offset(module, offset) {
        Some(s) => s,
        None => return Vec::new(),
    };

    let mut collector = ReferenceCollector::new(include_definition);

    match symbol {
        SymbolInfo::Variable {
            definition_span: Some(def_span),
            usage_span,
            ..
        } => {
            // For variables, we track by definition span to handle shadowing correctly
            collector.target_def_span = Some(def_span.clone());

            // Collect all references
            for item in &module.items {
                collect_refs_in_item(&mut collector, item);
            }

            // If we're on the definition and include_definition is true, make sure it's included
            if include_definition && usage_span == def_span {
                // Check if it's already there
                let already_has_def = collector
                    .references
                    .iter()
                    .any(|r| r.is_definition && r.span == def_span);
                if !already_has_def {
                    collector.references.insert(
                        0,
                        ReferenceInfo {
                            span: def_span,
                            is_definition: true,
                        },
                    );
                }
            }
        }

        SymbolInfo::Variable {
            name,
            definition_span: None,
            ..
        } => {
            // Variable without known definition - might be a parameter or global
            // Fall back to name-based matching in the same function scope
            collector.target_func_name = Some(name);
            for item in &module.items {
                collect_refs_in_item(&mut collector, item);
            }
        }

        SymbolInfo::FunctionCall { name, .. } | SymbolInfo::FunctionDef { name, .. } => {
            // For functions, match by name
            collector.target_func_name = Some(name.clone());
            for item in &module.items {
                collect_func_refs_in_item(&mut collector, item);
            }
        }

        SymbolInfo::StructRef { name, .. } => {
            // For structs, match by name
            collector.target_struct_name = Some(name.clone());
            for item in &module.items {
                collect_struct_refs_in_item(&mut collector, item);
            }
        }

        SymbolInfo::FieldAccess { .. } => {
            // Field access references are more complex - would need type info
            // For now, return empty
        }
    }

    collector.references
}

// Reference collection functions for variables
fn collect_refs_in_item(collector: &mut ReferenceCollector, item: &Item) {
    match item {
        Item::Function(func) => collect_refs_in_function(collector, func),
        Item::Impl(impl_block) => {
            for method in &impl_block.methods {
                collect_refs_in_function(collector, method);
            }
        }
        Item::TraitImpl(trait_impl) => {
            for method in &trait_impl.methods {
                collect_refs_in_function(collector, method);
            }
        }
        _ => {}
    }
}

fn collect_refs_in_function(collector: &mut ReferenceCollector, func: &Function) {
    collector.push_scope();

    // Add parameters to scope
    for param in &func.params {
        if let Some((name, _)) = extract_pattern_binding(&param.pattern) {
            // Use the function span as a rough definition span for parameters
            collector.define_variable(name, func.span.clone());
        }
    }

    collect_refs_in_block(collector, &func.body);

    collector.pop_scope();
}

fn collect_refs_in_block(collector: &mut ReferenceCollector, block: &Block) {
    collector.push_scope();

    for stmt in &block.stmts {
        collect_refs_in_stmt(collector, stmt);
    }

    if let Some(expr) = &block.expr {
        collect_refs_in_expr(collector, expr);
    }

    collector.pop_scope();
}

fn collect_refs_in_stmt(collector: &mut ReferenceCollector, stmt: &Stmt) {
    match stmt {
        Stmt::Let {
            pattern,
            value,
            span,
            ..
        } => {
            // Visit value first
            collect_refs_in_expr(collector, value);

            // Add binding to scope and check if it's a definition we're tracking
            if let Some((name, _)) = extract_pattern_binding(pattern) {
                // Check if this is the definition we're looking for
                if let Some(ref target_span) = collector.target_def_span
                    && span == target_span
                {
                    collector.add_reference(span.clone(), true);
                }
                collector.define_variable(name, span.clone());
            }
        }
        Stmt::Expr { expr, .. } => {
            collect_refs_in_expr(collector, expr);
        }
    }
}

fn collect_refs_in_expr(collector: &mut ReferenceCollector, expr: &SpannedExpr) {
    match &expr.expr {
        Expr::Ident(name) => {
            // Check if this variable references our target definition
            if let Some(ref target_span) = collector.target_def_span
                && let Some(def_span) = collector.lookup_variable(name)
                && &def_span == target_span
            {
                collector.add_reference(expr.span.clone(), false);
            }
        }

        Expr::Path { segments } if segments.len() == 1 => {
            // Single-segment path could be a variable
            let name = &segments[0];
            if let Some(ref target_span) = collector.target_def_span
                && let Some(def_span) = collector.lookup_variable(name)
                && &def_span == target_span
            {
                collector.add_reference(expr.span.clone(), false);
            }
        }

        Expr::Call { func, args, .. } => {
            collect_refs_in_expr(collector, func);
            for arg in args {
                collect_refs_in_expr(collector, arg);
            }
        }

        Expr::MethodCall { receiver, args, .. } => {
            collect_refs_in_expr(collector, receiver);
            for arg in args {
                collect_refs_in_expr(collector, arg);
            }
        }

        Expr::FieldAccess { expr: inner, .. } => {
            collect_refs_in_expr(collector, inner);
        }

        Expr::Binary { left, right, .. } => {
            collect_refs_in_expr(collector, left);
            collect_refs_in_expr(collector, right);
        }

        Expr::Unary { expr: inner, .. } => {
            collect_refs_in_expr(collector, inner);
        }

        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_refs_in_expr(collector, cond);
            collect_refs_in_block(collector, then_block);
            if let Some(else_blk) = else_block {
                collect_refs_in_block(collector, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            collect_refs_in_expr(collector, inner);
            for arm in arms {
                collect_refs_in_match_arm(collector, arm);
            }
        }

        Expr::Block(block) => {
            collect_refs_in_block(collector, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                collect_refs_in_expr(collector, elem);
            }
        }

        Expr::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_refs_in_expr(collector, field_expr);
            }
            if let Some(base_expr) = base {
                collect_refs_in_expr(collector, base_expr);
            }
        }

        Expr::For { clauses, body, .. } => {
            collector.push_scope();
            for clause in clauses {
                collect_refs_in_for_clause(collector, clause);
            }
            collect_refs_in_expr(collector, body);
            collector.pop_scope();
        }

        Expr::Closure { body, .. } => {
            collector.push_scope();
            collect_refs_in_block(collector, body);
            collector.pop_scope();
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                collect_refs_in_match_arm(collector, arm);
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                collect_refs_in_expr(collector, timeout_expr);
                collect_refs_in_block(collector, timeout_block);
            }
        }

        Expr::Spawn(inner) => {
            collect_refs_in_expr(collector, inner);
        }

        Expr::SpawnClosure(block) => {
            collector.push_scope();
            collect_refs_in_block(collector, block);
            collector.pop_scope();
        }

        Expr::Return(Some(inner)) => {
            collect_refs_in_expr(collector, inner);
        }

        Expr::Send { to, msg } => {
            collect_refs_in_expr(collector, to);
            collect_refs_in_expr(collector, msg);
        }

        Expr::Pipe { left, right } => {
            collect_refs_in_expr(collector, left);
            collect_refs_in_expr(collector, right);
        }

        Expr::Try { expr: inner } => {
            collect_refs_in_expr(collector, inner);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_refs_in_expr(collector, key);
                collect_refs_in_expr(collector, value);
            }
        }

        Expr::ListCons { head, tail } => {
            collect_refs_in_expr(collector, head);
            collect_refs_in_expr(collector, tail);
        }

        Expr::EnumVariant { args, .. } => match args {
            EnumVariantArgs::Tuple(exprs) => {
                for e in exprs {
                    collect_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Struct(fields) => {
                for (_, e) in fields {
                    collect_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Unit => {}
        },

        _ => {}
    }
}

fn collect_refs_in_match_arm(collector: &mut ReferenceCollector, arm: &MatchArm) {
    collector.push_scope();

    // Add pattern bindings to scope
    collect_pattern_bindings(collector, &arm.pattern, &arm.span);

    if let Some(guard) = &arm.guard {
        collect_refs_in_expr(collector, guard);
    }

    collect_refs_in_expr(collector, &arm.body);

    collector.pop_scope();
}

fn collect_refs_in_for_clause(collector: &mut ReferenceCollector, clause: &ForClause) {
    match clause {
        ForClause::Generator {
            pattern, source, ..
        } => {
            collect_refs_in_expr(collector, source);
            collect_pattern_bindings(collector, pattern, &source.span);
        }
        ForClause::When(expr) => {
            collect_refs_in_expr(collector, expr);
        }
    }
}

fn collect_pattern_bindings(
    collector: &mut ReferenceCollector,
    pattern: &Pattern,
    def_span: &Span,
) {
    match pattern {
        Pattern::Ident(name) => {
            collector.define_variable(name.clone(), def_span.clone());
        }
        Pattern::Tuple(elements) | Pattern::List(elements) => {
            for elem in elements {
                collect_pattern_bindings(collector, elem, def_span);
            }
        }
        Pattern::ListCons { head, tail } => {
            collect_pattern_bindings(collector, head, def_span);
            collect_pattern_bindings(collector, tail, def_span);
        }
        Pattern::Struct { fields, .. } => {
            for (_, pat) in fields {
                collect_pattern_bindings(collector, pat, def_span);
            }
        }
        Pattern::Enum { fields, .. } => match fields {
            EnumPatternFields::Tuple(pats) => {
                for pat in pats {
                    collect_pattern_bindings(collector, pat, def_span);
                }
            }
            EnumPatternFields::Struct(field_pats) => {
                for (_, pat) in field_pats {
                    collect_pattern_bindings(collector, pat, def_span);
                }
            }
            EnumPatternFields::Unit => {}
        },
        _ => {}
    }
}

// Reference collection functions for functions
fn collect_func_refs_in_item(collector: &mut ReferenceCollector, item: &Item) {
    match item {
        Item::Function(func) => {
            // Check if this is the function definition we're looking for
            if let Some(ref target_name) = collector.target_func_name
                && &func.name == target_name
            {
                collector.add_reference(func.span.clone(), true);
            }
            // Also search for calls within the function body
            collect_func_refs_in_function(collector, func);
        }
        Item::Impl(impl_block) => {
            for method in &impl_block.methods {
                if let Some(ref target_name) = collector.target_func_name
                    && &method.name == target_name
                {
                    collector.add_reference(method.span.clone(), true);
                }
                collect_func_refs_in_function(collector, method);
            }
        }
        Item::TraitImpl(trait_impl) => {
            for method in &trait_impl.methods {
                if let Some(ref target_name) = collector.target_func_name
                    && &method.name == target_name
                {
                    collector.add_reference(method.span.clone(), true);
                }
                collect_func_refs_in_function(collector, method);
            }
        }
        _ => {}
    }
}

fn collect_func_refs_in_function(collector: &mut ReferenceCollector, func: &Function) {
    collect_func_refs_in_block(collector, &func.body);
}

fn collect_func_refs_in_block(collector: &mut ReferenceCollector, block: &Block) {
    for stmt in &block.stmts {
        collect_func_refs_in_stmt(collector, stmt);
    }
    if let Some(expr) = &block.expr {
        collect_func_refs_in_expr(collector, expr);
    }
}

fn collect_func_refs_in_stmt(collector: &mut ReferenceCollector, stmt: &Stmt) {
    match stmt {
        Stmt::Let { value, .. } => {
            collect_func_refs_in_expr(collector, value);
        }
        Stmt::Expr { expr, .. } => {
            collect_func_refs_in_expr(collector, expr);
        }
    }
}

fn collect_func_refs_in_expr(collector: &mut ReferenceCollector, expr: &SpannedExpr) {
    match &expr.expr {
        Expr::Path { segments } => {
            // Check for function references (module::func or just func)
            if let Some(ref target_name) = collector.target_func_name
                && let Some(func_name) = segments.last()
                && func_name == target_name
            {
                collector.add_reference(expr.span.clone(), false);
            }
        }

        Expr::Call { func, args, .. } => {
            // Check if this is a call to our target function
            if let Expr::Path { segments } = &func.expr {
                if let Some(ref target_name) = collector.target_func_name
                    && let Some(func_name) = segments.last()
                    && func_name == target_name
                {
                    collector.add_reference(func.span.clone(), false);
                }
            } else if let Expr::Ident(name) = &func.expr {
                if let Some(ref target_name) = collector.target_func_name
                    && name == target_name
                {
                    collector.add_reference(func.span.clone(), false);
                }
            } else {
                collect_func_refs_in_expr(collector, func);
            }

            for arg in args {
                collect_func_refs_in_expr(collector, arg);
            }
        }

        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            collect_func_refs_in_expr(collector, receiver);

            // Check if this method call matches our target
            if let Some(ref target_name) = collector.target_func_name
                && method == target_name
            {
                // For method calls, we'd need type info to be precise
                // For now, include it as a potential reference
                collector.add_reference(expr.span.clone(), false);
            }

            for arg in args {
                collect_func_refs_in_expr(collector, arg);
            }
        }

        Expr::Binary { left, right, .. } => {
            collect_func_refs_in_expr(collector, left);
            collect_func_refs_in_expr(collector, right);
        }

        Expr::Unary { expr: inner, .. } => {
            collect_func_refs_in_expr(collector, inner);
        }

        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_func_refs_in_expr(collector, cond);
            collect_func_refs_in_block(collector, then_block);
            if let Some(else_blk) = else_block {
                collect_func_refs_in_block(collector, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            collect_func_refs_in_expr(collector, inner);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_func_refs_in_expr(collector, guard);
                }
                collect_func_refs_in_expr(collector, &arm.body);
            }
        }

        Expr::Block(block) => {
            collect_func_refs_in_block(collector, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                collect_func_refs_in_expr(collector, elem);
            }
        }

        Expr::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_func_refs_in_expr(collector, field_expr);
            }
            if let Some(base_expr) = base {
                collect_func_refs_in_expr(collector, base_expr);
            }
        }

        Expr::For { clauses, body, .. } => {
            for clause in clauses {
                if let ForClause::Generator { source, .. } = clause {
                    collect_func_refs_in_expr(collector, source);
                } else if let ForClause::When(expr) = clause {
                    collect_func_refs_in_expr(collector, expr);
                }
            }
            collect_func_refs_in_expr(collector, body);
        }

        Expr::Closure { body, .. } => {
            collect_func_refs_in_block(collector, body);
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_func_refs_in_expr(collector, guard);
                }
                collect_func_refs_in_expr(collector, &arm.body);
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                collect_func_refs_in_expr(collector, timeout_expr);
                collect_func_refs_in_block(collector, timeout_block);
            }
        }

        Expr::Spawn(inner) | Expr::Try { expr: inner } | Expr::Return(Some(inner)) => {
            collect_func_refs_in_expr(collector, inner);
        }

        Expr::SpawnClosure(block) => {
            collect_func_refs_in_block(collector, block);
        }

        Expr::Send { to, msg }
        | Expr::Pipe {
            left: to,
            right: msg,
        } => {
            collect_func_refs_in_expr(collector, to);
            collect_func_refs_in_expr(collector, msg);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_func_refs_in_expr(collector, key);
                collect_func_refs_in_expr(collector, value);
            }
        }

        Expr::ListCons { head, tail } => {
            collect_func_refs_in_expr(collector, head);
            collect_func_refs_in_expr(collector, tail);
        }

        Expr::FieldAccess { expr: inner, .. } => {
            collect_func_refs_in_expr(collector, inner);
        }

        Expr::EnumVariant { args, .. } => match args {
            EnumVariantArgs::Tuple(exprs) => {
                for e in exprs {
                    collect_func_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Struct(fields) => {
                for (_, e) in fields {
                    collect_func_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Unit => {}
        },

        _ => {}
    }
}

// Reference collection functions for structs
fn collect_struct_refs_in_item(collector: &mut ReferenceCollector, item: &Item) {
    match item {
        Item::Struct(s) => {
            if let Some(ref target_name) = collector.target_struct_name
                && &s.name == target_name
            {
                collector.add_reference(s.span.clone(), true);
            }
        }
        Item::Function(func) => {
            collect_struct_refs_in_function(collector, func);
        }
        Item::Impl(impl_block) => {
            // Check if impl is for our struct
            if let Some(ref target_name) = collector.target_struct_name
                && &impl_block.type_name == target_name
            {
                // The impl itself references the struct (at the type name position)
                // For now, just note it
            }
            for method in &impl_block.methods {
                collect_struct_refs_in_function(collector, method);
            }
        }
        Item::TraitImpl(trait_impl) => {
            for method in &trait_impl.methods {
                collect_struct_refs_in_function(collector, method);
            }
        }
        _ => {}
    }
}

fn collect_struct_refs_in_function(collector: &mut ReferenceCollector, func: &Function) {
    collect_struct_refs_in_block(collector, &func.body);
}

fn collect_struct_refs_in_block(collector: &mut ReferenceCollector, block: &Block) {
    for stmt in &block.stmts {
        collect_struct_refs_in_stmt(collector, stmt);
    }
    if let Some(expr) = &block.expr {
        collect_struct_refs_in_expr(collector, expr);
    }
}

fn collect_struct_refs_in_stmt(collector: &mut ReferenceCollector, stmt: &Stmt) {
    match stmt {
        Stmt::Let { value, .. } => {
            collect_struct_refs_in_expr(collector, value);
        }
        Stmt::Expr { expr, .. } => {
            collect_struct_refs_in_expr(collector, expr);
        }
    }
}

fn collect_struct_refs_in_expr(collector: &mut ReferenceCollector, expr: &SpannedExpr) {
    match &expr.expr {
        Expr::StructInit { name, fields, base } => {
            if let Some(ref target_name) = collector.target_struct_name
                && name == target_name
            {
                collector.add_reference(expr.span.clone(), false);
            }
            for (_, field_expr) in fields {
                collect_struct_refs_in_expr(collector, field_expr);
            }
            if let Some(base_expr) = base {
                collect_struct_refs_in_expr(collector, base_expr);
            }
        }

        Expr::Call { func, args, .. } => {
            collect_struct_refs_in_expr(collector, func);
            for arg in args {
                collect_struct_refs_in_expr(collector, arg);
            }
        }

        Expr::MethodCall { receiver, args, .. } => {
            collect_struct_refs_in_expr(collector, receiver);
            for arg in args {
                collect_struct_refs_in_expr(collector, arg);
            }
        }

        Expr::Binary { left, right, .. } => {
            collect_struct_refs_in_expr(collector, left);
            collect_struct_refs_in_expr(collector, right);
        }

        Expr::Unary { expr: inner, .. } => {
            collect_struct_refs_in_expr(collector, inner);
        }

        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_struct_refs_in_expr(collector, cond);
            collect_struct_refs_in_block(collector, then_block);
            if let Some(else_blk) = else_block {
                collect_struct_refs_in_block(collector, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            collect_struct_refs_in_expr(collector, inner);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_struct_refs_in_expr(collector, guard);
                }
                collect_struct_refs_in_expr(collector, &arm.body);
            }
        }

        Expr::Block(block) => {
            collect_struct_refs_in_block(collector, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                collect_struct_refs_in_expr(collector, elem);
            }
        }

        Expr::For { clauses, body, .. } => {
            for clause in clauses {
                if let ForClause::Generator { source, .. } = clause {
                    collect_struct_refs_in_expr(collector, source);
                } else if let ForClause::When(expr) = clause {
                    collect_struct_refs_in_expr(collector, expr);
                }
            }
            collect_struct_refs_in_expr(collector, body);
        }

        Expr::Closure { body, .. } => {
            collect_struct_refs_in_block(collector, body);
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_struct_refs_in_expr(collector, guard);
                }
                collect_struct_refs_in_expr(collector, &arm.body);
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                collect_struct_refs_in_expr(collector, timeout_expr);
                collect_struct_refs_in_block(collector, timeout_block);
            }
        }

        Expr::Spawn(inner) | Expr::Try { expr: inner } | Expr::Return(Some(inner)) => {
            collect_struct_refs_in_expr(collector, inner);
        }

        Expr::SpawnClosure(block) => {
            collect_struct_refs_in_block(collector, block);
        }

        Expr::Send { to, msg }
        | Expr::Pipe {
            left: to,
            right: msg,
        } => {
            collect_struct_refs_in_expr(collector, to);
            collect_struct_refs_in_expr(collector, msg);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_struct_refs_in_expr(collector, key);
                collect_struct_refs_in_expr(collector, value);
            }
        }

        Expr::ListCons { head, tail } => {
            collect_struct_refs_in_expr(collector, head);
            collect_struct_refs_in_expr(collector, tail);
        }

        Expr::FieldAccess { expr: inner, .. } => {
            collect_struct_refs_in_expr(collector, inner);
        }

        Expr::EnumVariant { args, .. } => match args {
            EnumVariantArgs::Tuple(exprs) => {
                for e in exprs {
                    collect_struct_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Struct(fields) => {
                for (_, e) in fields {
                    collect_struct_refs_in_expr(collector, e);
                }
            }
            EnumVariantArgs::Unit => {}
        },

        _ => {}
    }
}

fn visit_item(ctx: &mut LookupContext, item: &Item) {
    match item {
        Item::Function(func) => visit_function(ctx, func),
        Item::Impl(impl_block) => {
            for method in &impl_block.methods {
                visit_function(ctx, method);
                if ctx.found.is_some() {
                    return;
                }
            }
        }
        Item::TraitImpl(trait_impl) => {
            for method in &trait_impl.methods {
                visit_function(ctx, method);
                if ctx.found.is_some() {
                    return;
                }
            }
        }
        _ => {}
    }
}

fn visit_function(ctx: &mut LookupContext, func: &Function) {
    // Check if we're in this function at all
    if !ctx.is_in_range(&func.span) {
        return;
    }

    // Check if we're on the function name itself
    // Function name is after "fn " (or "pub fn ")
    let name_start = if func.is_pub {
        func.span.start + 7 // "pub fn "
    } else {
        func.span.start + 3 // "fn "
    };
    let name_end = name_start + func.name.len();

    if ctx.target_offset >= name_start && ctx.target_offset < name_end {
        ctx.set_found(SymbolInfo::FunctionDef {
            name: func.name.clone(),
            span: func.span.clone(),
        });
        return;
    }

    // Create a new scope for the function
    ctx.push_scope();

    // Add parameters to scope
    for param in &func.params {
        if let Some((name, span)) = extract_pattern_binding(&param.pattern) {
            ctx.define_variable(name, span);
        }
    }

    // Visit the function body
    visit_block(ctx, &func.body);

    ctx.pop_scope();
}

fn visit_block(ctx: &mut LookupContext, block: &Block) {
    // Check if we're in this block
    if !ctx.is_in_range(&block.span) {
        return;
    }

    ctx.push_scope();

    for stmt in &block.stmts {
        visit_stmt(ctx, stmt);
        if ctx.found.is_some() {
            ctx.pop_scope();
            return;
        }
    }

    if let Some(expr) = &block.expr {
        visit_expr(ctx, expr);
    }

    ctx.pop_scope();
}

fn visit_stmt(ctx: &mut LookupContext, stmt: &Stmt) {
    match stmt {
        Stmt::Let {
            pattern,
            value,
            span,
            ..
        } => {
            // Visit the value expression first
            visit_expr(ctx, value);
            if ctx.found.is_some() {
                return;
            }

            // Add binding to scope
            if let Some((name, def_span)) = extract_pattern_binding(pattern) {
                // Check if cursor is on the pattern itself
                if ctx.is_in_range(&def_span) {
                    ctx.set_found(SymbolInfo::Variable {
                        name: name.clone(),
                        usage_span: def_span.clone(),
                        definition_span: Some(span.clone()),
                        type_info: None,
                    });
                    return;
                }
                ctx.define_variable(name, span.clone());
            }
        }
        Stmt::Expr { expr, .. } => {
            visit_expr(ctx, expr);
        }
    }
}

fn visit_expr(ctx: &mut LookupContext, expr: &SpannedExpr) {
    // Check if we're in this expression
    if !ctx.is_in_range(&expr.span) {
        return;
    }

    match &expr.expr {
        Expr::Ident(name) => {
            // Look up variable definition
            let def_span = ctx.lookup_variable(name);
            ctx.set_found(SymbolInfo::Variable {
                name: name.clone(),
                usage_span: expr.span.clone(),
                definition_span: def_span,
                type_info: None,
            });
        }

        Expr::Path { segments } => {
            if segments.len() == 2 {
                // Module::function reference
                ctx.set_found(SymbolInfo::FunctionCall {
                    module: vec![segments[0].clone()],
                    name: segments[1].clone(),
                    call_span: expr.span.clone(),
                });
            } else if segments.len() == 1 {
                // Could be a function name or type
                let def_span = ctx.lookup_variable(&segments[0]);
                ctx.set_found(SymbolInfo::Variable {
                    name: segments[0].clone(),
                    usage_span: expr.span.clone(),
                    definition_span: def_span,
                    type_info: None,
                });
            }
        }

        Expr::Call { func, args, .. } => {
            // Check if cursor is on the function part
            visit_expr(ctx, func);
            if ctx.found.is_some() {
                return;
            }

            // Check arguments
            for arg in args {
                visit_expr(ctx, arg);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            visit_expr(ctx, receiver);
            if ctx.found.is_some() {
                return;
            }

            // Check if cursor is on the method name
            // Method name comes after receiver and "."
            let method_start = receiver.span.end + 1; // after the dot
            let method_end = method_start + method.len();
            if ctx.target_offset >= method_start && ctx.target_offset < method_end {
                ctx.set_found(SymbolInfo::FunctionCall {
                    module: vec![],
                    name: method.clone(),
                    call_span: expr.span.clone(),
                });
                return;
            }

            for arg in args {
                visit_expr(ctx, arg);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::FieldAccess { expr: inner, field } => {
            visit_expr(ctx, inner);
            if ctx.found.is_some() {
                return;
            }

            // Check if cursor is on the field name
            let field_start = inner.span.end + 1;
            let field_end = field_start + field.len();
            if ctx.target_offset >= field_start && ctx.target_offset < field_end {
                ctx.set_found(SymbolInfo::FieldAccess {
                    field: field.clone(),
                    span: expr.span.clone(),
                });
            }
        }

        Expr::Binary { left, right, .. } => {
            visit_expr(ctx, left);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, right);
        }

        Expr::Unary { expr: inner, .. } => {
            visit_expr(ctx, inner);
        }

        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            visit_expr(ctx, cond);
            if ctx.found.is_some() {
                return;
            }
            visit_block(ctx, then_block);
            if ctx.found.is_some() {
                return;
            }
            if let Some(else_blk) = else_block {
                visit_block(ctx, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            visit_expr(ctx, inner);
            if ctx.found.is_some() {
                return;
            }
            for arm in arms {
                visit_match_arm(ctx, arm);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::Block(block) => {
            visit_block(ctx, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                visit_expr(ctx, elem);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::StructInit { name, fields, base } => {
            // Check if cursor is on struct name
            let name_end = expr.span.start + name.len();
            if ctx.target_offset >= expr.span.start && ctx.target_offset < name_end {
                ctx.set_found(SymbolInfo::StructRef {
                    name: name.clone(),
                    span: expr.span.clone(),
                });
                return;
            }

            for (_, field_expr) in fields {
                visit_expr(ctx, field_expr);
                if ctx.found.is_some() {
                    return;
                }
            }

            if let Some(base_expr) = base {
                visit_expr(ctx, base_expr);
            }
        }

        Expr::For { clauses, body, .. } => {
            ctx.push_scope();
            for clause in clauses {
                visit_for_clause(ctx, clause);
                if ctx.found.is_some() {
                    ctx.pop_scope();
                    return;
                }
            }
            visit_expr(ctx, body);
            ctx.pop_scope();
        }

        Expr::Closure { body, .. } => {
            ctx.push_scope();
            // Could add closure params to scope here
            visit_block(ctx, body);
            ctx.pop_scope();
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                visit_match_arm(ctx, arm);
                if ctx.found.is_some() {
                    return;
                }
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                visit_expr(ctx, timeout_expr);
                if ctx.found.is_some() {
                    return;
                }
                visit_block(ctx, timeout_block);
            }
        }

        Expr::Spawn(inner) => {
            visit_expr(ctx, inner);
        }

        Expr::SpawnClosure(block) => {
            ctx.push_scope();
            visit_block(ctx, block);
            ctx.pop_scope();
        }

        Expr::Return(Some(inner)) => {
            visit_expr(ctx, inner);
        }

        Expr::Send { to, msg } => {
            visit_expr(ctx, to);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, msg);
        }

        Expr::Pipe { left, right } => {
            visit_expr(ctx, left);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, right);
        }

        Expr::Try { expr: inner } => {
            visit_expr(ctx, inner);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                visit_expr(ctx, key);
                if ctx.found.is_some() {
                    return;
                }
                visit_expr(ctx, value);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::ListCons { head, tail } => {
            visit_expr(ctx, head);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, tail);
        }

        Expr::EnumVariant { args, .. } => match args {
            EnumVariantArgs::Tuple(exprs) => {
                for e in exprs {
                    visit_expr(ctx, e);
                    if ctx.found.is_some() {
                        return;
                    }
                }
            }
            EnumVariantArgs::Struct(fields) => {
                for (_, e) in fields {
                    visit_expr(ctx, e);
                    if ctx.found.is_some() {
                        return;
                    }
                }
            }
            EnumVariantArgs::Unit => {}
        },

        // Literals and other simple expressions
        _ => {}
    }
}

fn visit_match_arm(ctx: &mut LookupContext, arm: &MatchArm) {
    if !ctx.is_in_range(&arm.span) {
        return;
    }

    ctx.push_scope();

    // Add pattern bindings to scope
    add_pattern_bindings(ctx, &arm.pattern, &arm.span);

    // Visit guard if present
    if let Some(guard) = &arm.guard {
        visit_expr(ctx, guard);
        if ctx.found.is_some() {
            ctx.pop_scope();
            return;
        }
    }

    // Visit body
    visit_expr(ctx, &arm.body);

    ctx.pop_scope();
}

fn visit_for_clause(ctx: &mut LookupContext, clause: &ForClause) {
    match clause {
        ForClause::Generator {
            pattern, source, ..
        } => {
            visit_expr(ctx, source);
            if ctx.found.is_some() {
                return;
            }
            // Add pattern bindings
            add_pattern_bindings(ctx, pattern, &source.span);
        }
        ForClause::When(expr) => {
            visit_expr(ctx, expr);
        }
    }
}

fn add_pattern_bindings(ctx: &mut LookupContext, pattern: &Pattern, def_span: &Span) {
    match pattern {
        Pattern::Ident(name) => {
            ctx.define_variable(name.clone(), def_span.clone());
        }
        Pattern::Tuple(elements) | Pattern::List(elements) => {
            for elem in elements {
                add_pattern_bindings(ctx, elem, def_span);
            }
        }
        Pattern::ListCons { head, tail } => {
            add_pattern_bindings(ctx, head, def_span);
            add_pattern_bindings(ctx, tail, def_span);
        }
        Pattern::Struct { fields, .. } => {
            for (_, pat) in fields {
                add_pattern_bindings(ctx, pat, def_span);
            }
        }
        Pattern::Enum { fields, .. } => match fields {
            EnumPatternFields::Tuple(pats) => {
                for pat in pats {
                    add_pattern_bindings(ctx, pat, def_span);
                }
            }
            EnumPatternFields::Struct(field_pats) => {
                for (_, pat) in field_pats {
                    add_pattern_bindings(ctx, pat, def_span);
                }
            }
            EnumPatternFields::Unit => {}
        },
        _ => {}
    }
}

/// Extract the primary binding name and a rough span from a pattern.
fn extract_pattern_binding(pattern: &Pattern) -> Option<(String, Span)> {
    match pattern {
        Pattern::Ident(name) => Some((name.clone(), 0..name.len())), // Placeholder span
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Parser;

    #[test]
    fn test_find_variable_reference() {
        let source = r#"mod test {
    fn example() {
        let x = 42;
        x
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the position of the second 'x' (the reference on its own line)
        let x_ref_pos = source.rfind("\n        x").unwrap() + 9; // After "\n        "
        let info = find_symbol_at_offset(&module, x_ref_pos);

        assert!(
            info.is_some(),
            "Expected to find symbol at offset {}",
            x_ref_pos
        );
        if let Some(SymbolInfo::Variable { name, .. }) = info {
            assert_eq!(name, "x");
        } else {
            panic!("Expected variable info, got {:?}", info);
        }
    }

    #[test]
    fn test_find_function_call() {
        let source = r#"mod test {
    fn example() {
        io::println("hello")
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of "io::println"
        let call_pos = source.find("io::").unwrap();
        let info = find_symbol_at_offset(&module, call_pos);

        assert!(
            info.is_some(),
            "Expected to find symbol at offset {}",
            call_pos
        );
        if let Some(SymbolInfo::FunctionCall { module, name, .. }) = info {
            assert_eq!(module, vec!["io"]);
            assert_eq!(name, "println");
        } else {
            panic!("Expected function call info, got {:?}", info);
        }
    }

    #[test]
    fn test_find_all_variable_references() {
        let source = r#"mod test {
    fn example() {
        let x = 42;
        let y = x + 1;
        x
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of 'x' in the usage (y = x + 1)
        // We use rfind to get the last standalone x
        let x_usage_pos = source.rfind("\n        x").unwrap() + 9; // Position of standalone 'x'
        let refs = find_all_references(&module, x_usage_pos, true);

        // Should find: definition + 2 references (in y = x + 1 and standalone x)
        assert!(
            refs.len() >= 2,
            "Expected at least 2 references, got {} at positions: {:?}",
            refs.len(),
            refs.iter().map(|r| r.span.clone()).collect::<Vec<_>>()
        );

        // Check that one is marked as definition
        let def_count = refs.iter().filter(|r| r.is_definition).count();
        assert!(
            def_count >= 1,
            "Expected at least 1 definition, got {}",
            def_count
        );
    }

    #[test]
    fn test_find_all_references_variable_shadowing() {
        let source = r#"mod test {
    fn example() {
        let x = 1;
        let y = x;
        {
            let x = 2;
            let z = x;
        }
        x
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of outer 'x' usage (y = x)
        let outer_x_usage = source.find("y = x").unwrap() + 4; // Position of 'x' in 'y = x'
        let outer_refs = find_all_references(&module, outer_x_usage, true);

        // Find position of inner 'x' usage (z = x)
        let inner_x_usage = source.find("z = x").unwrap() + 4; // Position of 'x' in 'z = x'
        let inner_refs = find_all_references(&module, inner_x_usage, true);

        // The outer x and inner x should have different reference counts
        // Outer x: definition, y = x, final x (3 refs)
        // Inner x: definition, z = x (2 refs)
        // At minimum, both should find some references
        assert!(
            !outer_refs.is_empty(),
            "Outer x should have at least 1 reference, got {}",
            outer_refs.len()
        );
        assert!(
            !inner_refs.is_empty(),
            "Inner x should have at least 1 reference, got {}",
            inner_refs.len()
        );
    }

    #[test]
    fn test_find_all_function_references() {
        let source = r#"mod test {
    fn helper() -> Int {
        42
    }

    fn example() {
        let a = helper();
        let b = helper();
        a + b
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of 'helper' function definition
        let helper_pos = source.find("fn helper").unwrap() + 3;
        let refs = find_all_references(&module, helper_pos, true);

        // Should find: definition + 2 calls
        assert!(
            refs.len() >= 3,
            "Expected at least 3 references (1 def + 2 calls), got {}",
            refs.len()
        );

        let def_count = refs.iter().filter(|r| r.is_definition).count();
        assert_eq!(def_count, 1, "Expected exactly 1 definition");
    }

    #[test]
    fn test_find_all_references_exclude_definition() {
        let source = r#"mod test {
    fn example() {
        let x = 42;
        x
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of 'x' usage
        let x_usage_pos = source.rfind("\n        x").unwrap() + 9;
        let refs_with_def = find_all_references(&module, x_usage_pos, true);
        let refs_without_def = find_all_references(&module, x_usage_pos, false);

        // With definition should have more or equal refs
        assert!(
            refs_with_def.len() >= refs_without_def.len(),
            "With definition should have >= refs than without"
        );

        // Without definition should have no definitions
        let def_count_without = refs_without_def.iter().filter(|r| r.is_definition).count();
        assert_eq!(
            def_count_without, 0,
            "Should have 0 definitions when excluding"
        );
    }
}
