//! Core Erlang code generator.
//!
//! This module generates Core Erlang source code from the AST,
//! which can then be compiled to BEAM bytecode using `erlc +from_core`.

use crate::compiler::ast::{
    BinOp, BitEndianness, BitSegmentType, BitSignedness, BitStringSegment, Block, Expr, Function,
    Item, MatchArm, Module, Pattern, Stmt, UnaryOp,
};

/// Core Erlang emitter error.
#[derive(Debug, Clone)]
pub struct CoreErlangError {
    pub message: String,
}

impl CoreErlangError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for CoreErlangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Core Erlang error: {}", self.message)
    }
}

impl std::error::Error for CoreErlangError {}

pub type CoreErlangResult<T> = Result<T, CoreErlangError>;

/// Core Erlang code emitter.
pub struct CoreErlangEmitter {
    output: String,
    indent: usize,
    /// Counter for generating fresh variable names.
    #[allow(dead_code)]
    var_counter: usize,
    /// Current module name (needed for local function calls)
    #[allow(dead_code)]
    module_name: String,
}

impl CoreErlangEmitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            var_counter: 0,
            module_name: String::new(),
        }
    }

    /// Generate a fresh variable name.
    #[allow(dead_code)]
    fn fresh_var(&mut self) -> String {
        let name = format!("_@c{}", self.var_counter);
        self.var_counter += 1;
        name
    }

    /// Convert an identifier to Core Erlang variable format (capitalize first letter).
    fn var_name(name: &str) -> String {
        let mut chars: Vec<char> = name.chars().collect();
        if !chars.is_empty() {
            chars[0] = chars[0].to_ascii_uppercase();
        }
        // Prefix with _ if it starts with a number or is a reserved word
        let result: String = chars.into_iter().collect();
        if result.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
            format!("_{}", result)
        } else {
            result
        }
    }

    /// Check if a function name is a built-in function (BIF).
    fn is_bif(name: &str) -> bool {
        matches!(
            name,
            "self"
                | "is_atom"
                | "is_binary"
                | "is_integer"
                | "is_float"
                | "is_list"
                | "is_tuple"
                | "is_pid"
                | "is_reference"
                | "is_function"
                | "is_map"
                | "is_boolean"
                | "is_number"
                | "hd"
                | "tl"
                | "length"
                | "tuple_size"
                | "map_size"
                | "element"
                | "setelement"
                | "make_ref"
                | "throw"
                | "error"
                | "exit"
                | "abs"
                | "trunc"
                | "round"
                | "float"
                | "atom_to_list"
                | "list_to_atom"
                | "integer_to_list"
                | "list_to_integer"
                | "float_to_list"
                | "list_to_float"
                | "tuple_to_list"
                | "list_to_tuple"
                | "binary_to_list"
                | "list_to_binary"
                | "term_to_binary"
                | "binary_to_term"
                | "size"
                | "byte_size"
                | "bit_size"
                | "node"
                | "nodes"
                | "now"
                | "time"
                | "date"
                | "register"
                | "unregister"
                | "whereis"
                | "registered"
                | "link"
                | "unlink"
                | "spawn_link"
                | "monitor"
                | "demonitor"
                | "process_info"
                | "processes"
                | "put"
                | "get"
                | "erase"
                | "get_keys"
        )
    }

    /// Emit a string to the output.
    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Emit a newline and indentation.
    fn newline(&mut self) {
        self.emit("\n");
        for _ in 0..self.indent {
            self.emit("    ");
        }
    }

    /// Emit a complete Core Erlang module.
    pub fn emit_module(&mut self, module: &Module) -> CoreErlangResult<String> {
        self.module_name = module.name.clone();

        // Module header
        self.emit(&format!("module '{}'", module.name));

        // Collect exported functions
        let exports: Vec<String> = module
            .items
            .iter()
            .filter_map(|item| {
                if let Item::Function(f) = item {
                    if f.is_pub {
                        Some(format!("'{}'/{}", f.name, f.params.len()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        self.emit(" [");
        self.emit(&exports.join(", "));
        self.emit("]");

        self.newline();
        self.emit("    attributes []");
        self.newline();

        // Emit each function
        for item in &module.items {
            if let Item::Function(f) = item {
                self.newline();
                self.emit_function(f)?;
            }
        }

        self.newline();
        self.emit("end");
        self.newline();

        Ok(self.output.clone())
    }

    /// Emit a function definition.
    fn emit_function(&mut self, func: &Function) -> CoreErlangResult<()> {
        let arity = func.params.len();
        self.emit(&format!("'{}'/{} =", func.name, arity));
        self.newline();
        self.indent += 1;

        self.emit("fun (");

        // Emit parameters
        let param_names: Vec<String> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                // Use pattern if it's an identifier, otherwise generate a name
                match &p.pattern {
                    Pattern::Ident(name) => Self::var_name(name),
                    _ => format!("_@p{}", i),
                }
            })
            .collect();
        self.emit(&param_names.join(", "));
        self.emit(") ->");
        self.newline();

        self.indent += 1;
        self.emit_block(&func.body)?;
        self.indent -= 1;

        self.indent -= 1;
        Ok(())
    }

    /// Emit a block (statements + optional expression).
    fn emit_block(&mut self, block: &Block) -> CoreErlangResult<()> {
        // In Core Erlang, we need to convert statements to nested let expressions
        self.emit_block_inner(&block.stmts, &block.expr)
    }

    /// Recursively emit statements as nested let expressions.
    fn emit_block_inner(
        &mut self,
        stmts: &[Stmt],
        final_expr: &Option<Box<Expr>>,
    ) -> CoreErlangResult<()> {
        if stmts.is_empty() {
            // No more statements, emit the final expression
            if let Some(expr) = final_expr {
                self.emit_expr(expr)?;
            } else {
                // Empty block returns 'ok'
                self.emit("'ok'");
            }
            return Ok(());
        }

        let (first, rest) = stmts.split_first().unwrap();

        match first {
            Stmt::Let { pattern, value, .. } => {
                self.emit("let <");
                self.emit_pattern(pattern)?;
                self.emit("> =");
                self.newline();
                self.indent += 1;
                self.emit_expr(value)?;
                self.indent -= 1;
                self.newline();
                self.emit("in ");
                self.emit_block_inner(rest, final_expr)?;
            }
            Stmt::Expr(expr) => {
                if rest.is_empty() && final_expr.is_none() {
                    // Last expression statement is the result
                    self.emit_expr(expr)?;
                } else {
                    // Expression statement - bind to _ and continue
                    self.emit("let <_> =");
                    self.newline();
                    self.indent += 1;
                    self.emit_expr(expr)?;
                    self.indent -= 1;
                    self.newline();
                    self.emit("in ");
                    self.emit_block_inner(rest, final_expr)?;
                }
            }
        }

        Ok(())
    }

    /// Emit an expression.
    fn emit_expr(&mut self, expr: &Expr) -> CoreErlangResult<()> {
        match expr {
            Expr::Int(n) => {
                self.emit(&n.to_string());
            }

            Expr::String(s) => {
                // Core Erlang strings are lists of integers
                self.emit("[");
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&chars.join(", "));
                self.emit("]");
            }

            Expr::Atom(a) => {
                self.emit(&format!("'{}'", a));
            }

            Expr::Bool(b) => {
                self.emit(if *b { "'true'" } else { "'false'" });
            }

            Expr::Ident(name) => {
                self.emit(&Self::var_name(name));
            }

            Expr::Unit => {
                // Unit is represented as empty tuple or 'ok'
                self.emit("'ok'");
            }

            Expr::Binary { op, left, right } => {
                self.emit_binary_op(*op, left, right)?;
            }

            Expr::Unary { op, expr } => {
                match op {
                    UnaryOp::Neg => {
                        self.emit("call 'erlang':'-'(0, ");
                        self.emit_expr(expr)?;
                        self.emit(")");
                    }
                    UnaryOp::Not => {
                        self.emit("call 'erlang':'not'(");
                        self.emit_expr(expr)?;
                        self.emit(")");
                    }
                }
            }

            Expr::Call { func, args } => {
                // Check if it's a local function call or external
                match func.as_ref() {
                    Expr::Ident(name) => {
                        // Check if it's a BIF (built-in function)
                        if Self::is_bif(name) {
                            self.emit(&format!("call 'erlang':'{}'(", name));
                            self.emit_args(args)?;
                            self.emit(")");
                        } else {
                            // Local function call
                            self.emit(&format!("apply '{}'/{}", name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        }
                    }
                    Expr::Path { segments } if segments.len() == 2 => {
                        // Module:Function call
                        self.emit(&format!(
                            "call '{}':'{}'",
                            segments[0].to_lowercase(),
                            segments[1]
                        ));
                        self.emit("(");
                        self.emit_args(args)?;
                        self.emit(")");
                    }
                    _ => {
                        // Higher-order function application
                        self.emit("apply ");
                        self.emit_expr(func)?;
                        self.emit("(");
                        self.emit_args(args)?;
                        self.emit(")");
                    }
                }
            }

            Expr::If {
                cond,
                then_block,
                else_block,
            } => {
                // Convert if to case on boolean
                self.emit("case ");
                self.emit_expr(cond)?;
                self.emit(" of");
                self.newline();
                self.indent += 1;

                self.emit("<'true'> when 'true' ->");
                self.newline();
                self.indent += 1;
                self.emit_block(then_block)?;
                self.indent -= 1;
                self.newline();

                self.emit("<'false'> when 'true' ->");
                self.newline();
                self.indent += 1;
                if let Some(else_blk) = else_block {
                    self.emit_block(else_blk)?;
                } else {
                    self.emit("'ok'");
                }
                self.indent -= 1;

                self.indent -= 1;
                self.newline();
                self.emit("end");
            }

            Expr::Match { expr, arms } => {
                self.emit("case ");
                self.emit_expr(expr)?;
                self.emit(" of");
                self.newline();
                self.indent += 1;

                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        self.newline();
                    }
                    self.emit_match_arm(arm)?;
                }

                self.indent -= 1;
                self.newline();
                self.emit("end");
            }

            Expr::Block(block) => {
                self.emit_block(block)?;
            }

            Expr::Tuple(elements) => {
                self.emit("{");
                self.emit_args(elements)?;
                self.emit("}");
            }

            Expr::List(elements) => {
                if elements.is_empty() {
                    self.emit("[]");
                } else {
                    self.emit("[");
                    self.emit_args(elements)?;
                    self.emit("]");
                }
            }

            Expr::Spawn(expr) => {
                // Core Erlang requires binding the fun to a variable first
                let tmp_var = self.fresh_var();
                self.emit(&format!("let <{}> =", tmp_var));
                self.newline();
                self.indent += 1;
                self.emit("fun () ->");
                self.newline();
                self.indent += 1;
                self.emit_expr(expr)?;
                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'erlang':'spawn'({})", tmp_var));
            }

            Expr::SpawnClosure(block) => {
                // Core Erlang requires binding the fun to a variable first
                let tmp_var = self.fresh_var();
                self.emit(&format!("let <{}> =", tmp_var));
                self.newline();
                self.indent += 1;
                self.emit("fun () ->");
                self.newline();
                self.indent += 1;
                self.emit_block(block)?;
                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'erlang':'spawn'({})", tmp_var));
            }

            Expr::Send { to, msg } => {
                self.emit("call 'erlang':'!'(");
                self.emit_expr(to)?;
                self.emit(", ");
                self.emit_expr(msg)?;
                self.emit(")");
            }

            Expr::Receive { arms, timeout } => {
                self.emit("receive");
                self.newline();
                self.indent += 1;

                for (i, arm) in arms.iter().enumerate() {
                    self.emit_match_arm(arm)?;
                    if i < arms.len() - 1 {
                        self.newline();
                    }
                }

                self.indent -= 1;
                self.newline();

                if let Some((time_expr, after_block)) = timeout {
                    self.emit("after ");
                    self.emit_expr(time_expr)?;
                    self.emit(" ->");
                    self.newline();
                    self.indent += 1;
                    self.emit_block(after_block)?;
                    self.indent -= 1;
                    self.newline();
                }

                self.emit("end");
            }

            Expr::Return(opt_expr) => {
                // Core Erlang doesn't have explicit return, just emit the expression
                if let Some(e) = opt_expr {
                    self.emit_expr(e)?;
                } else {
                    self.emit("'ok'");
                }
            }

            Expr::BitString(segments) => {
                self.emit("#{");
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_bitstring_segment_expr(seg)?;
                }
                self.emit("}#");
            }

            Expr::MethodCall { receiver: _, method, args: _ } => {
                // Method calls need to be translated - for now treat as function call
                // In real usage, this might need more sophisticated handling
                return Err(CoreErlangError::new(format!(
                    "Method calls not yet supported: .{}()",
                    method
                )));
            }

            Expr::StructInit { name: _, fields } => {
                // Structs become maps in Erlang
                self.emit("~{");
                for (i, (field_name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("'{}' => ", field_name));
                    self.emit_expr(value)?;
                }
                self.emit("}~");
            }

            Expr::EnumVariant { type_name: _, variant, args } => {
                // Enum variants become tagged tuples: {variant, arg1, arg2, ...}
                if args.is_empty() {
                    self.emit(&format!("'{}'", variant.to_lowercase()));
                } else {
                    self.emit("{");
                    self.emit(&format!("'{}'", variant.to_lowercase()));
                    for arg in args {
                        self.emit(", ");
                        self.emit_expr(arg)?;
                    }
                    self.emit("}");
                }
            }

            Expr::FieldAccess { expr, field } => {
                // Map field access
                self.emit("call 'maps':'get'('");
                self.emit(field);
                self.emit("', ");
                self.emit_expr(expr)?;
                self.emit(")");
            }

            Expr::Path { segments } => {
                // Module path - emit as atom or function reference
                if segments.len() == 1 {
                    self.emit(&format!("'{}'", segments[0]));
                } else {
                    // Module:function reference
                    self.emit(&format!(
                        "fun '{}':'{}'",
                        segments[0].to_lowercase(),
                        segments[1]
                    ));
                }
            }
        }

        Ok(())
    }

    /// Emit a binary operation.
    fn emit_binary_op(&mut self, op: BinOp, left: &Expr, right: &Expr) -> CoreErlangResult<()> {
        let erlang_op = match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "div", // Integer division in Erlang
            BinOp::Mod => "rem",
            BinOp::Eq => "=:=",  // Exact equality
            BinOp::Ne => "=/=",  // Exact inequality
            BinOp::Lt => "<",
            BinOp::Le => "=<",   // Note: Erlang uses =< not <=
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
        };

        self.emit(&format!("call 'erlang':'{}'(", erlang_op));
        self.emit_expr(left)?;
        self.emit(", ");
        self.emit_expr(right)?;
        self.emit(")");

        Ok(())
    }

    /// Emit function arguments.
    fn emit_args(&mut self, args: &[Expr]) -> CoreErlangResult<()> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit_expr(arg)?;
        }
        Ok(())
    }

    /// Emit a match arm.
    fn emit_match_arm(&mut self, arm: &MatchArm) -> CoreErlangResult<()> {
        self.emit("<");
        self.emit_pattern(&arm.pattern)?;
        self.emit("> when ");

        if let Some(guard) = &arm.guard {
            self.emit_expr(guard)?;
        } else {
            self.emit("'true'");
        }

        self.emit(" ->");
        self.newline();
        self.indent += 1;
        self.emit_expr(&arm.body)?;
        self.indent -= 1;

        Ok(())
    }

    /// Emit a pattern.
    fn emit_pattern(&mut self, pattern: &Pattern) -> CoreErlangResult<()> {
        match pattern {
            Pattern::Wildcard => {
                self.emit("_");
            }

            Pattern::Ident(name) => {
                self.emit(&Self::var_name(name));
            }

            Pattern::Int(n) => {
                self.emit(&n.to_string());
            }

            Pattern::String(s) => {
                // String pattern as list of chars
                self.emit("[");
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&chars.join(", "));
                self.emit("]");
            }

            Pattern::Atom(a) => {
                self.emit(&format!("'{}'", a));
            }

            Pattern::Bool(b) => {
                self.emit(if *b { "'true'" } else { "'false'" });
            }

            Pattern::Tuple(elements) => {
                self.emit("{");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_pattern(elem)?;
                }
                self.emit("}");
            }

            Pattern::List(elements) => {
                if elements.is_empty() {
                    self.emit("[]");
                } else {
                    self.emit("[");
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.emit_pattern(elem)?;
                    }
                    self.emit("]");
                }
            }

            Pattern::ListCons { head, tail } => {
                self.emit("[");
                self.emit_pattern(head)?;
                self.emit("|");
                self.emit_pattern(tail)?;
                self.emit("]");
            }

            Pattern::Struct { name: _, fields } => {
                // Struct patterns become map patterns
                self.emit("~{");
                for (i, (field_name, pat)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("'{}' := ", field_name));
                    self.emit_pattern(pat)?;
                }
                self.emit("}~");
            }

            Pattern::Enum { name: _, variant, fields } => {
                // Enum patterns become tuple patterns
                if fields.is_empty() {
                    self.emit(&format!("'{}'", variant.to_lowercase()));
                } else {
                    self.emit("{");
                    self.emit(&format!("'{}'", variant.to_lowercase()));
                    for field in fields {
                        self.emit(", ");
                        self.emit_pattern(field)?;
                    }
                    self.emit("}");
                }
            }

            Pattern::BitString(segments) => {
                self.emit("#{");
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_bitstring_segment_pattern(seg)?;
                }
                self.emit("}#");
            }
        }

        Ok(())
    }

    /// Emit a bitstring segment in an expression context.
    fn emit_bitstring_segment_expr(
        &mut self,
        seg: &BitStringSegment<Box<Expr>>,
    ) -> CoreErlangResult<()> {
        self.emit("#<");
        self.emit_expr(&seg.value)?;
        self.emit(">(");

        // Size
        if let Some(size) = &seg.size {
            self.emit_expr(size)?;
        } else {
            self.emit("8"); // default size
        }

        // Type specifiers
        self.emit(", ");
        self.emit("1, "); // unit = 1 bit

        let type_spec = match seg.segment_type {
            BitSegmentType::Integer => "'integer'",
            BitSegmentType::Float => "'float'",
            BitSegmentType::Binary => "'binary'",
            BitSegmentType::Utf8 => "'utf8'",
        };
        self.emit(type_spec);

        self.emit(", [");
        let mut specs = Vec::new();
        match seg.endianness {
            BitEndianness::Big => specs.push("'big'"),
            BitEndianness::Little => specs.push("'little'"),
        }
        match seg.signedness {
            BitSignedness::Unsigned => specs.push("'unsigned'"),
            BitSignedness::Signed => specs.push("'signed'"),
        }
        self.emit(&specs.join(", "));
        self.emit("])");

        Ok(())
    }

    /// Emit a bitstring segment in a pattern context.
    fn emit_bitstring_segment_pattern(
        &mut self,
        seg: &BitStringSegment<Box<Pattern>>,
    ) -> CoreErlangResult<()> {
        self.emit("#<");
        self.emit_pattern(&seg.value)?;
        self.emit(">(");

        // Size
        if let Some(size) = &seg.size {
            self.emit_expr(size)?;
        } else {
            match seg.segment_type {
                BitSegmentType::Binary => self.emit("'all'"),
                _ => self.emit("8"),
            }
        }

        // Type specifiers
        self.emit(", ");
        self.emit("1, "); // unit = 1 bit

        let type_spec = match seg.segment_type {
            BitSegmentType::Integer => "'integer'",
            BitSegmentType::Float => "'float'",
            BitSegmentType::Binary => "'binary'",
            BitSegmentType::Utf8 => "'utf8'",
        };
        self.emit(type_spec);

        self.emit(", [");
        let mut specs = Vec::new();
        match seg.endianness {
            BitEndianness::Big => specs.push("'big'"),
            BitEndianness::Little => specs.push("'little'"),
        }
        match seg.signedness {
            BitSignedness::Unsigned => specs.push("'unsigned'"),
            BitSignedness::Signed => specs.push("'signed'"),
        }
        self.emit(&specs.join(", "));
        self.emit("])");

        Ok(())
    }
}

impl Default for CoreErlangEmitter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to compile source code to Core Erlang.
pub fn emit_core_erlang(source: &str) -> CoreErlangResult<String> {
    use crate::compiler::parser::Parser;

    let mut parser = Parser::new(source);
    let module = parser
        .parse_module()
        .map_err(|e| CoreErlangError::new(format!("Parse error: {}", e.message)))?;

    let mut emitter = CoreErlangEmitter::new();
    emitter.emit_module(&module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let source = r#"
            mod test {
                pub fn add(a: int, b: int) -> int {
                    a + b
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("module 'test'"));
        assert!(result.contains("'add'/2"));
        assert!(result.contains("call 'erlang':'+'"));
    }

    #[test]
    fn test_let_binding() {
        let source = r#"
            mod test {
                pub fn example() -> int {
                    let x = 1;
                    let y = 2;
                    x + y
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("let <X>"));
        assert!(result.contains("let <Y>"));
        assert!(result.contains("in"));
    }

    #[test]
    fn test_match_expression() {
        let source = r#"
            mod test {
                pub fn check(n: int) -> atom {
                    match n {
                        0 => :zero,
                        _ => :other,
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("case"));
        assert!(result.contains("'zero'"));
        assert!(result.contains("'other'"));
    }

    #[test]
    fn test_spawn_and_send() {
        let source = r#"
            mod test {
                pub fn start() -> pid {
                    spawn(loop())
                }

                fn loop() -> int {
                    0
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'erlang':'spawn'"));
    }
}
