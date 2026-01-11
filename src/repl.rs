//! Dream interactive shell (REPL)
//!
//! Provides an interactive environment for evaluating Dream expressions
//! using the BEAM runtime with a persistent process for fast evaluation.

use std::cell::RefCell;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, ExitCode, Stdio};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Context, Editor, Helper};

use dream::compiler::{BinOp, Expr, Parser, StringPart};

/// Counter for generating unique module names
static EVAL_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Erlang eval server code - runs in a loop reading filenames and evaluating them
/// Protocol: after evaluation, prints "\x00DREAM_RESULT\x00" marker, then "ok:value" or "err:reason"
const RESULT_MARKER: &str = "\x00DREAM_RESULT\x00";
const EVAL_SERVER: &str = r#"Loop = fun Loop() -> case io:get_line("") of eof -> ok; {error, _} -> ok; Line -> Filename = string:trim(Line), case Filename of "" -> Loop(); _ -> Result = try ModName = list_to_atom(filename:basename(Filename, ".core")), case compile:file(Filename, [from_core, binary, return_errors]) of {ok, ModName, Binary} -> code:purge(ModName), case code:load_binary(ModName, Filename, Binary) of {module, ModName} -> Val = ModName:'__eval__'(), code:purge(ModName), code:delete(ModName), {ok, Val}; {error, What} -> {error, {load_failed, What}} end; {error, Errors, _Warnings} -> {error, {compile_failed, Errors}} end catch Class:Reason:Stack -> {error, {Class, Reason, Stack}} end, io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]), case Result of {ok, Value} -> io:format("ok:~p~n", [Value]); {error, Err} -> io:format("err:~p~n", [Err]) end, Loop() end end end, Loop()"#;

/// Keywords for completion
const KEYWORDS: &[&str] = &[
    "let", "if", "else", "match", "fn", "pub", "mod", "use", "struct", "enum", "trait", "impl",
    "type", "extern", "spawn", "send", "receive", "self", "true", "false",
];

/// REPL commands for completion
const COMMANDS: &[&str] = &[":help", ":quit", ":q", ":clear", ":bindings", ":b", ":h"];

/// Standard library modules for completion
const STDLIB_MODULES: &[&str] = &[
    "string", "list", "io", "map", "file", "process", "timer", "math", "binary", "tuple",
    "genserver", "supervisor", "application", "ets", "agent", "task",
];

/// Binding stored from a let statement
#[derive(Clone, Debug)]
struct Binding {
    name: String,
    /// The Core Erlang expression for this binding's value
    core_expr: String,
}

/// Shared state for completion
type SharedBindings = Rc<RefCell<Vec<Binding>>>;

/// REPL helper for rustyline (completion, hints, etc.)
struct ReplHelper {
    bindings: SharedBindings,
}

impl ReplHelper {
    fn new(bindings: SharedBindings) -> Self {
        Self { bindings }
    }
}

impl Completer for ReplHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let mut completions = Vec::new();

        // Find the word being typed
        let (start, word) = find_word_start(line, pos);

        // Complete REPL commands
        if word.starts_with(':') {
            for cmd in COMMANDS {
                if cmd.starts_with(word) {
                    completions.push(Pair {
                        display: cmd.to_string(),
                        replacement: cmd.to_string(),
                    });
                }
            }
            return Ok((start, completions));
        }

        // Complete module paths (e.g., "string::" -> "string::reverse")
        if let Some(colon_pos) = word.find("::") {
            let module = &word[..colon_pos];
            let func_prefix = &word[colon_pos + 2..];

            // Get functions for the module
            if let Some(funcs) = get_module_functions(module) {
                for func in funcs {
                    if func.starts_with(func_prefix) {
                        let full = format!("{}::{}", module, func);
                        completions.push(Pair {
                            display: full.clone(),
                            replacement: full,
                        });
                    }
                }
            }
            return Ok((start, completions));
        }

        // Complete keywords
        for kw in KEYWORDS {
            if kw.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: kw.to_string(),
                    replacement: kw.to_string(),
                });
            }
        }

        // Complete binding names
        let bindings = self.bindings.borrow();
        for binding in bindings.iter() {
            if binding.name.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: binding.name.clone(),
                    replacement: binding.name.clone(),
                });
            }
        }

        // Complete module names (with :: suffix hint)
        for module in STDLIB_MODULES {
            if module.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: format!("{}::", module),
                    replacement: format!("{}::", module),
                });
            }
        }

        Ok((start, completions))
    }
}

impl Hinter for ReplHelper {
    type Hint = String;
}

impl Highlighter for ReplHelper {}

impl Validator for ReplHelper {}

impl Helper for ReplHelper {}

/// Find the start of the word at position
fn find_word_start(line: &str, pos: usize) -> (usize, &str) {
    let line = &line[..pos];
    let start = line
        .rfind(|c: char| c.is_whitespace() || c == '(' || c == '[' || c == '{' || c == ',')
        .map(|i| i + 1)
        .unwrap_or(0);
    (start, &line[start..])
}

/// Get known functions for a module
fn get_module_functions(module: &str) -> Option<&'static [&'static str]> {
    match module {
        "string" => Some(&[
            "len",
            "concat",
            "contains",
            "starts_with",
            "ends_with",
            "to_upper",
            "to_lower",
            "trim",
            "trim_left",
            "trim_right",
            "split",
            "join",
            "replace",
            "slice",
            "reverse",
            "is_empty",
            "repeat",
            "pad_left",
            "pad_right",
            "from_int",
            "to_int",
            "from_atom",
            "to_atom",
        ]),
        "list" => Some(&[
            "reverse",
            "length",
            "append",
            "head",
            "tail",
            "map",
            "filter",
            "foldl",
            "foldr",
            "member",
            "nth",
            "take",
            "drop",
            "zip",
            "flatten",
            "sort",
        ]),
        "io" => Some(&["println", "print", "read_line", "format"]),
        "map" => Some(&[
            "new", "get", "put", "remove", "keys", "values", "size", "has_key", "merge", "to_list",
        ]),
        "file" => Some(&[
            "read",
            "write",
            "exists",
            "delete",
            "rename",
            "list_dir",
            "is_dir",
            "is_file",
        ]),
        "process" => Some(&[
            "spawn",
            "spawn_link",
            "send",
            "self",
            "exit",
            "link",
            "unlink",
            "monitor",
            "demonitor",
            "register",
            "whereis",
            "registered",
        ]),
        "timer" => Some(&["sleep", "send_after", "apply_after", "cancel"]),
        "math" => Some(&[
            "abs", "floor", "ceil", "round", "sqrt", "pow", "sin", "cos", "tan", "log", "exp",
            "min", "max",
        ]),
        _ => None,
    }
}

/// REPL state
struct ReplState {
    /// Accumulated bindings from let statements (shared with completer)
    bindings: SharedBindings,
    /// The running BEAM process
    beam_process: Option<Child>,
    /// Stdin handle for the BEAM process
    beam_stdin: Option<std::process::ChildStdin>,
    /// Stdout reader for the BEAM process
    beam_stdout: Option<BufReader<std::process::ChildStdout>>,
    /// Path to stdlib beam files
    stdlib_path: Option<String>,
    /// Temp directory for this session
    temp_dir: std::path::PathBuf,
}

impl ReplState {
    fn new() -> Self {
        let stdlib_path = find_stdlib_path();
        let temp_dir = std::env::temp_dir();

        Self {
            bindings: Rc::new(RefCell::new(Vec::new())),
            beam_process: None,
            beam_stdin: None,
            beam_stdout: None,
            stdlib_path,
            temp_dir,
        }
    }

    /// Start the BEAM process if not already running
    fn ensure_beam_running(&mut self) -> Result<(), String> {
        if self.beam_process.is_some() {
            return Ok(());
        }

        // The eval server code is self-contained
        let eval_code = format!("{}.", EVAL_SERVER);

        let mut cmd = Command::new("erl");
        cmd.arg("-noshell");

        // Add temp dir to code path
        cmd.arg("-pa").arg(&self.temp_dir);

        // Add stdlib to code path if available
        if let Some(ref stdlib) = self.stdlib_path {
            cmd.arg("-pa").arg(stdlib);
        }

        cmd.arg("-eval").arg(&eval_code);

        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut child = cmd
            .spawn()
            .map_err(|e| format!("Failed to start BEAM: {}", e))?;

        let stdin = child.stdin.take().ok_or("Failed to get stdin")?;
        let stdout = child.stdout.take().ok_or("Failed to get stdout")?;

        self.beam_process = Some(child);
        self.beam_stdin = Some(stdin);
        self.beam_stdout = Some(BufReader::new(stdout));

        Ok(())
    }

    /// Evaluate an expression and return the result as a string
    fn eval_expr(&mut self, expr: &Expr) -> Result<String, String> {
        // Ensure BEAM is running
        self.ensure_beam_running()?;

        // Generate a unique module name
        let counter = EVAL_COUNTER.fetch_add(1, Ordering::SeqCst);
        let module_name = format!("dream_repl_{}", counter);

        // Generate Core Erlang for the expression wrapped in a module
        let core_erlang = self.generate_core_erlang(&module_name, expr)?;

        // Write to temp file
        let core_file = self.temp_dir.join(format!("{}.core", module_name));

        std::fs::write(&core_file, &core_erlang)
            .map_err(|e| format!("Failed to write Core Erlang: {}", e))?;

        // Send filename to BEAM process
        let stdin = self.beam_stdin.as_mut().ok_or("BEAM stdin not available")?;
        writeln!(stdin, "{}", core_file.display())
            .map_err(|e| format!("Failed to send to BEAM: {}", e))?;
        stdin
            .flush()
            .map_err(|e| format!("Failed to flush: {}", e))?;

        // Read result from BEAM process
        // First, read until we see the result marker (printing any output along the way)
        let stdout = self
            .beam_stdout
            .as_mut()
            .ok_or("BEAM stdout not available")?;

        loop {
            let mut line = String::new();
            stdout
                .read_line(&mut line)
                .map_err(|e| format!("Failed to read from BEAM: {}", e))?;

            if line.is_empty() {
                // Clean up temp file
                let _ = std::fs::remove_file(&core_file);
                return Err("No response from BEAM".to_string());
            }

            let trimmed = line.trim();
            if trimmed == RESULT_MARKER {
                // Found marker, next line is the result
                break;
            }

            // This is output from io::println or similar - print it
            print!("{}", line);
        }

        // Read the actual result line
        let mut result_line = String::new();
        stdout
            .read_line(&mut result_line)
            .map_err(|e| format!("Failed to read result from BEAM: {}", e))?;

        // Clean up temp file
        let _ = std::fs::remove_file(&core_file);

        // Parse result - format is "ok:value" or "err:reason"
        let result_line = result_line.trim();
        if let Some(value) = result_line.strip_prefix("ok:") {
            Ok(format_dream_value(value))
        } else if let Some(err) = result_line.strip_prefix("err:") {
            Err(format!("Evaluation error: {}", err))
        } else {
            Err(format!("Invalid result format: {}", result_line))
        }
    }

    /// Generate Core Erlang for an expression wrapped in a module
    fn generate_core_erlang(&self, module_name: &str, expr: &Expr) -> Result<String, String> {
        let mut output = String::new();

        // Module header
        output.push_str(&format!(
            "module '{}' ['__eval__'/0]\n    attributes []\n\n",
            module_name
        ));

        // Generate the eval function
        output.push_str("'__eval__'/0 =\nfun () ->\n");

        // Add bindings as let expressions
        let bindings = self.bindings.borrow();
        for binding in bindings.iter() {
            output.push_str(&format!(
                "    let <{}> =\n    {}\n    in ",
                capitalize_first(&binding.name),
                binding.core_expr
            ));
        }

        // Generate the expression
        let expr_core = self.expr_to_core(expr)?;
        output.push_str(&expr_core);
        output.push_str("\nend\n");

        Ok(output)
    }

    /// Convert an expression to Core Erlang
    fn expr_to_core(&self, expr: &Expr) -> Result<String, String> {
        let bindings = self.bindings.borrow();
        expr_to_core_inner(expr, &bindings)
    }

    /// Add a binding
    fn add_binding(&mut self, name: String, expr: &Expr) -> Result<(), String> {
        let core_expr = self.expr_to_core(expr)?;
        let mut bindings = self.bindings.borrow_mut();
        // Remove existing binding with same name (shadowing)
        bindings.retain(|b| b.name != name);
        bindings.push(Binding { name, core_expr });
        Ok(())
    }

    /// Clear all bindings
    fn clear_bindings(&mut self) {
        self.bindings.borrow_mut().clear();
    }
}

/// Convert an expression to Core Erlang (standalone function for borrowing)
fn expr_to_core_inner(expr: &Expr, bindings: &[Binding]) -> Result<String, String> {
    match expr {
        Expr::Int(n) => Ok(n.to_string()),
        Expr::Bool(b) => Ok(if *b { "'true'" } else { "'false'" }.to_string()),
        Expr::String(s) => {
            // Convert string to list of integers (Erlang string representation)
            let chars: Vec<String> = s.bytes().map(|b| b.to_string()).collect();
            Ok(format!("[{}]", chars.join(", ")))
        }
        Expr::Atom(a) => Ok(format!("'{}'", a)),
        Expr::Ident(name) => {
            // Check if it's a binding
            if bindings.iter().any(|b| &b.name == name) {
                Ok(capitalize_first(name))
            } else {
                Err(format!("Undefined variable: {}", name))
            }
        }
        Expr::Binary { op, left, right } => {
            let left_core = expr_to_core_inner(left, bindings)?;
            let right_core = expr_to_core_inner(right, bindings)?;
            let op_str = match op {
                BinOp::Add => "call 'erlang':'+'",
                BinOp::Sub => "call 'erlang':'-'",
                BinOp::Mul => "call 'erlang':'*'",
                BinOp::Div => "call 'erlang':'div'",
                BinOp::Mod => "call 'erlang':'rem'",
                BinOp::Eq => "call 'erlang':'=:='",
                BinOp::Ne => "call 'erlang':'=/='",
                BinOp::Lt => "call 'erlang':'<'",
                BinOp::Le => "call 'erlang':'=<'",
                BinOp::Gt => "call 'erlang':'>'",
                BinOp::Ge => "call 'erlang':'>='",
                BinOp::And => "call 'erlang':'and'",
                BinOp::Or => "call 'erlang':'or'",
            };
            Ok(format!("{}({}, {})", op_str, left_core, right_core))
        }
        Expr::Tuple(elems) => {
            let elem_strs: Result<Vec<_>, _> = elems
                .iter()
                .map(|e| expr_to_core_inner(e, bindings))
                .collect();
            Ok(format!("{{{}}}", elem_strs?.join(", ")))
        }
        Expr::List(elems) => {
            let elem_strs: Result<Vec<_>, _> = elems
                .iter()
                .map(|e| expr_to_core_inner(e, bindings))
                .collect();
            Ok(format!("[{}]", elem_strs?.join(", ")))
        }
        Expr::Call {
            func,
            args,
            type_args: _,
            inferred_type_args: _,
        } => {
            // Handle qualified calls like module::func or Struct::method
            if let Expr::Path { segments } = func.as_ref() {
                if segments.len() == 2 {
                    let first = &segments[0];
                    let func_name = &segments[1];
                    let arg_strs: Result<Vec<_>, _> = args
                        .iter()
                        .map(|a| expr_to_core_inner(a, bindings))
                        .collect();

                    // Check if first segment is PascalCase (struct method) or snake_case (module function)
                    let is_struct_method = first
                        .chars()
                        .next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false);

                    if is_struct_method {
                        // Struct::method() -> 'dream::struct':'Struct_method'()
                        let module = first.to_lowercase();
                        let compiled_func = format!("{}_{}", first, func_name);
                        return Ok(format!(
                            "call 'dream::{}'  :'{}'({})",
                            module,
                            compiled_func,
                            arg_strs?.join(", ")
                        ));
                    } else {
                        // module::func() -> 'dream::module':'func'()
                        return Ok(format!(
                            "call 'dream::{}'  :'{}'({})",
                            first,
                            func_name,
                            arg_strs?.join(", ")
                        ));
                    }
                }
            }

            // Simple function call
            if let Expr::Ident(name) = func.as_ref() {
                let arg_strs: Result<Vec<_>, _> = args
                    .iter()
                    .map(|a| expr_to_core_inner(a, bindings))
                    .collect();
                return Ok(format!("apply '{}'({})", name, arg_strs?.join(", ")));
            }

            Err(format!("Unsupported call expression: {:?}", func))
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            type_args: _,
            resolved_module: _,
            inferred_type_args: _,
        } => {
            // Method call: receiver.method(args) -> Struct_method(receiver, args)
            // We need to figure out the struct type from the receiver
            // For now, we'll try to infer it from the method name pattern
            let receiver_core = expr_to_core_inner(receiver, bindings)?;
            let mut all_args = vec![receiver_core];
            for arg in args {
                all_args.push(expr_to_core_inner(arg, bindings)?);
            }

            // Try to determine the struct type from the receiver
            // This is a heuristic - check if receiver is a binding with a known struct type
            // For now, we'll require the user to use Struct::method() syntax for struct methods
            // But we can try common patterns like Map, Option, Result

            // Check common struct methods
            let struct_methods: &[(&str, &str)] = &[
                // Map methods
                ("put", "Map"),
                ("get", "Map"),
                ("get_or", "Map"),
                ("has_key", "Map"),
                ("delete", "Map"),
                ("merge", "Map"),
                ("keys", "Map"),
                ("values", "Map"),
                ("to_list", "Map"),
                ("size", "Map"),
                ("is_empty", "Map"),
                ("fetch", "Map"),
                // Option methods
                ("unwrap", "Option"),
                ("unwrap_or", "Option"),
                ("is_some", "Option"),
                ("is_none", "Option"),
                ("map", "Option"),
                ("and_then", "Option"),
                // Result methods
                ("unwrap", "Result"),
                ("unwrap_or", "Result"),
                ("is_ok", "Result"),
                ("is_err", "Result"),
                ("ok", "Result"),
                ("err", "Result"),
            ];

            // Find matching struct for this method
            if let Some((_, struct_name)) = struct_methods.iter().find(|(m, _)| *m == method.as_str()) {
                let module = struct_name.to_lowercase();
                let compiled_func = format!("{}_{}", struct_name, method);
                return Ok(format!(
                    "call 'dream::{}'  :'{}'({})",
                    module,
                    compiled_func,
                    all_args.join(", ")
                ));
            }

            Err(format!(
                "Unknown method '{}'. Use Struct::method() syntax for struct methods.",
                method
            ))
        }
        Expr::StringInterpolation(parts) => {
            // Convert interpolated string to iolist and then binary
            // Result: call 'erlang':'iolist_to_binary'([part1, part2, ...])
            let mut part_strs = Vec::new();
            for part in parts {
                match part {
                    StringPart::Literal(s) => {
                        // Emit literal as char code list
                        let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                        part_strs.push(format!("[{}]", chars.join(", ")));
                    }
                    StringPart::Expr(e) => {
                        // Convert expression to string using display::to_string
                        let expr_core = expr_to_core_inner(e, bindings)?;
                        part_strs.push(format!("call 'dream::display':'to_string'({})", expr_core));
                    }
                }
            }
            Ok(format!(
                "call 'erlang':'iolist_to_binary'([{}])",
                part_strs.join(", ")
            ))
        }
        Expr::Pipe { left, right } => {
            // Transform `a |> f(b, c)` into `f(a, b, c)`
            let left_core = expr_to_core_inner(left, bindings)?;

            // Right side should be a call - inject left as first argument
            match right.as_ref() {
                Expr::Call {
                    func,
                    args,
                    type_args: _,
                    inferred_type_args: _,
                } => {
                    // Handle qualified calls like module::func or Struct::method
                    if let Expr::Path { segments } = func.as_ref() {
                        if segments.len() == 2 {
                            let first = &segments[0];
                            let func_name = &segments[1];
                            let mut all_args = vec![left_core];
                            for arg in args {
                                all_args.push(expr_to_core_inner(arg, bindings)?);
                            }

                            // Check if first segment is PascalCase (struct method) or snake_case (module function)
                            let is_struct_method = first
                                .chars()
                                .next()
                                .map(|c| c.is_uppercase())
                                .unwrap_or(false);

                            if is_struct_method {
                                // Struct::method() -> 'dream::struct':'Struct_method'()
                                let module = first.to_lowercase();
                                let compiled_func = format!("{}_{}", first, func_name);
                                return Ok(format!(
                                    "call 'dream::{}'  :'{}'({})",
                                    module,
                                    compiled_func,
                                    all_args.join(", ")
                                ));
                            } else {
                                // module::func() -> 'dream::module':'func'()
                                return Ok(format!(
                                    "call 'dream::{}'  :'{}'({})",
                                    first,
                                    func_name,
                                    all_args.join(", ")
                                ));
                            }
                        }
                    }

                    // Simple function call
                    if let Expr::Ident(name) = func.as_ref() {
                        let mut all_args = vec![left_core];
                        for arg in args {
                            all_args.push(expr_to_core_inner(arg, bindings)?);
                        }
                        return Ok(format!("apply '{}'({})", name, all_args.join(", ")));
                    }

                    Err(format!("Unsupported pipe target: {:?}", func))
                }
                _ => Err(format!(
                    "Pipe right side must be a function call: {:?}",
                    right
                )),
            }
        }
        _ => Err(format!("Unsupported expression type in REPL: {:?}", expr)),
    }
}

impl Drop for ReplState {
    fn drop(&mut self) {
        // Close stdin to signal EOF to the eval loop
        self.beam_stdin.take();

        // Kill the BEAM process if running
        if let Some(ref mut child) = self.beam_process {
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

/// Find the stdlib beam files
fn find_stdlib_path() -> Option<String> {
    // Try relative to executable
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let stdlib = exe_dir.join("../stdlib");
            if stdlib.exists() {
                return stdlib
                    .canonicalize()
                    .ok()
                    .map(|p| p.to_string_lossy().into_owned());
            }
        }
    }

    // Try target/stdlib
    let target_stdlib = std::path::Path::new("target/stdlib");
    if target_stdlib.exists() {
        return target_stdlib
            .canonicalize()
            .ok()
            .map(|p| p.to_string_lossy().into_owned());
    }

    None
}

/// Capitalize the first character of a string (for Erlang variable names)
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Format an Erlang value as Dream syntax
fn format_dream_value(value: &str) -> String {
    let value = value.trim();

    // Option: {some, X} -> Some(X), none -> None
    if value == "none" {
        return "None".to_string();
    }
    if let Some(inner) = value.strip_prefix("{some,").and_then(|s| s.strip_suffix('}')) {
        return format!("Some({})", format_dream_value(inner.trim()));
    }

    // Result: {ok, X} -> Ok(X), {error, X} -> Err(X)
    if let Some(inner) = value.strip_prefix("{ok,").and_then(|s| s.strip_suffix('}')) {
        return format!("Ok({})", format_dream_value(inner.trim()));
    }
    if let Some(inner) = value.strip_prefix("{error,").and_then(|s| s.strip_suffix('}')) {
        return format!("Err({})", format_dream_value(inner.trim()));
    }

    // Atoms: 'atom' or atom -> :atom
    if let Some(inner) = value.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
        // Skip special atoms
        if inner == "true" || inner == "false" || inner == "ok" || inner == "error" {
            return inner.to_string();
        }
        return format!(":{}", inner);
    }
    // Unquoted atoms (simple identifiers that aren't keywords)
    if value.chars().next().map(|c| c.is_lowercase()).unwrap_or(false)
        && value.chars().all(|c| c.is_alphanumeric() || c == '_')
        && value != "true"
        && value != "false"
        && value != "ok"
        && value != "error"
    {
        return format!(":{}", value);
    }

    // Binary strings: <<"text">> -> "text"
    if let Some(inner) = value.strip_prefix("<<\"").and_then(|s| s.strip_suffix("\">>")) {
        return format!("\"{}\"", inner);
    }

    // Structs: #{data => ..., '__struct__' => 'module::Name'} -> simplified
    // For now, just clean up the display a bit
    if value.starts_with("#{") && value.contains("'__struct__'") {
        // Extract struct name and data
        if let Some(struct_start) = value.find("'__struct__' => '") {
            let after_struct = &value[struct_start + 17..];
            if let Some(struct_end) = after_struct.find('\'') {
                let struct_path = &after_struct[..struct_end];
                // Extract just the struct name (after ::)
                let struct_name = struct_path.split("::").last().unwrap_or(struct_path);

                // Extract data field
                if let Some(data_start) = value.find("data => ") {
                    let after_data = &value[data_start + 8..];
                    // Find matching brace
                    if let Some(data_content) = extract_map_content(after_data) {
                        // For Map struct, use literal syntax { ... }
                        if struct_name == "Map" {
                            if data_content == "#{}" {
                                return "{}".to_string();
                            }
                            return format!("{{ {} }}", format_map_fields(&data_content));
                        }
                        // For other structs, use Struct(...) syntax
                        if data_content == "#{}" {
                            return format!("{}()", struct_name);
                        }
                        return format!("{}({})", struct_name, format_map_fields(&data_content));
                    }
                }
            }
        }
    }

    // Default: return as-is
    value.to_string()
}

/// Extract map content from a string starting with #{
fn extract_map_content(s: &str) -> Option<String> {
    if !s.starts_with("#{") {
        return None;
    }
    let mut depth = 0;
    let mut end_idx = 0;
    for (i, c) in s.chars().enumerate() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end_idx = i + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    if end_idx > 0 {
        Some(s[..end_idx].to_string())
    } else {
        None
    }
}

/// Format map fields for display
fn format_map_fields(map_str: &str) -> String {
    // Simple transformation: #{key => value} -> key: value
    let inner = map_str.strip_prefix("#{").and_then(|s| s.strip_suffix('}'));
    if let Some(inner) = inner {
        if inner.is_empty() {
            return String::new();
        }
        // This is a simplified version - just replace => with :
        inner.replace(" => ", ": ")
    } else {
        map_str.to_string()
    }
}

/// Print the welcome banner
fn print_banner() {
    println!("Dream {} (BEAM backend)", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :quit to exit");
    println!();
}

/// Print help information
fn print_help() {
    println!("Commands:");
    println!("  :help          Show this help message");
    println!("  :quit, :q      Exit the shell");
    println!("  :clear         Clear all bindings");
    println!("  :bindings      Show current bindings");
    println!();
    println!("Enter Dream expressions to evaluate them.");
    println!("Use 'let x = expr' to create bindings.");
    println!("Press TAB for completion.");
}

/// Run the interactive shell
pub fn run_shell() -> ExitCode {
    print_banner();

    let mut state = ReplState::new();

    // Create editor with completion helper
    let helper = ReplHelper::new(Rc::clone(&state.bindings));
    let mut rl = match Editor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize readline: {}", e);
            return ExitCode::from(1);
        }
    };
    rl.set_helper(Some(helper));

    loop {
        let readline = rl.readline("dream> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(line);

                // Handle special commands (only known commands, not atoms)
                if line.starts_with(':') {
                    match line {
                        ":quit" | ":q" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" => {
                            print_help();
                            continue;
                        }
                        ":clear" => {
                            state.clear_bindings();
                            println!("Bindings cleared.");
                            continue;
                        }
                        ":bindings" | ":b" => {
                            let bindings = state.bindings.borrow();
                            if bindings.is_empty() {
                                println!("No bindings.");
                            } else {
                                for binding in bindings.iter() {
                                    println!("  {} = <expr>", binding.name);
                                }
                            }
                            continue;
                        }
                        // If not a known command, treat as an expression (atom)
                        _ => {}
                    }
                }

                // Parse and evaluate
                match parse_and_eval(&mut state, line) {
                    Ok(result) => println!("{}", result),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    ExitCode::SUCCESS
}

/// Parse input and evaluate it
fn parse_and_eval(state: &mut ReplState, input: &str) -> Result<String, String> {
    // Try to parse as a let statement first
    if input.trim_start().starts_with("let ") {
        return parse_and_eval_let(state, input);
    }

    // Parse as an expression
    let mut parser = Parser::new(input);
    let expr = parser
        .parse_expr()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Evaluate
    state.eval_expr(&expr)
}

/// Parse and evaluate a let statement
fn parse_and_eval_let(state: &mut ReplState, input: &str) -> Result<String, String> {
    // Simple parsing: "let name = expr"
    let input = input.trim_start().strip_prefix("let ").unwrap();

    // Find the = sign
    let eq_pos = input
        .find('=')
        .ok_or_else(|| "Expected '=' in let statement".to_string())?;

    let name = input[..eq_pos].trim().to_string();
    let expr_str = input[eq_pos + 1..].trim();

    // Validate name
    if name.is_empty() || !name.chars().next().unwrap().is_alphabetic() {
        return Err("Invalid variable name".to_string());
    }

    // Parse the expression
    let mut parser = Parser::new(expr_str);
    let expr = parser
        .parse_expr()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Add binding
    state.add_binding(name, &expr)?;

    Ok(":ok".to_string())
}
