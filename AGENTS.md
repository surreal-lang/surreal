# Dream

A BEAM virtual machine in Rust that compiles to WebAssembly.

## Goal

Build a Rust-like syntax language that compiles to Dream bytecode, combining Rust's syntax with Erlang's concurrency semantics (processes, message passing, pattern matching).

## Architecture

```
Source Code → Lexer → Parser → AST → Codegen → Dream Bytecode
                                                      ↓
                                              Dream VM (Rust/WASM)
```

## Current VM Features

- **Processes**: spawn, spawn_link, parent tracking
- **Message Passing**: send, receive, receive_timeout, receive_match (selective)
- **Links & Monitors**: bidirectional crash notification, one-way DOWN messages
- **Process Registry**: register/unregister/whereis named processes
- **Data Types**: integers, atoms, tuples, lists, strings, PIDs
- **Pattern Matching**: wildcards, variables, literals, tuple/list destructuring
- **Control Flow**: jump, jump_if, jump_unless, call, return
- **Stack**: push, pop operations
- **Arithmetic**: add, sub, mul, div, mod
- **Comparisons**: eq, ne, lt, lte, gt, gte

## Building

```bash
# Run tests
cargo test

# Build WASM
wasm-pack build --target web
```

## Commands

- `cargo test` - run all tests
- `cargo check` - type check
- `wasm-pack build --target web` - build WASM package

---

# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
