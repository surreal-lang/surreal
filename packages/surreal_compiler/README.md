# Surreal Compiler

Erlang/OTP library for compiling Surreal source code to BEAM bytecode.

## Installation

Add `surreal_compiler` to your `rebar.config`:

```erlang
{deps, [
    {surreal_compiler, "0.1.0"}
]}.
```

Or in your `mix.exs`:

```elixir
def deps do
  [
    {:surreal_compiler, "~> 0.1.0"}
  ]
end
```

## Usage

### Parsing Surreal Code

```erlang
Source = <<"
    mod hello {
        pub fn add(a: int, b: int) -> int {
            a + b
        }
    }
">>,

{ok, Modules} = surreal_native:parse(Source).
%% Returns: [{<<"hello">>, [{<<"add">>, 2}]}]
```

### Generating Core Erlang AST

```erlang
{ok, CoreAST} = surreal_native:generate_core_ast(Source).
%% Returns the Core Erlang AST representation
```

### Compiling to BEAM

```erlang
%% Compile a Surreal project
surreal_compiler:compile_project("/path/to/project").

%% Compile a single file
surreal_compiler:compile_file("/path/to/file.surreal").
```

## NIF Handling

This library uses a Rust NIF for parsing and code generation. The NIF is **automatically downloaded** from GitHub releases on first use - no Rust toolchain required.

### Manual NIF Build (Development)

If you're developing the compiler, use the `dev` profile to build the NIF locally:

```bash
rebar3 as dev compile
```

This requires Rust to be installed.

## API Reference

### surreal_native

- `parse(Source)` - Parse Surreal source code, returns module information
- `generate_core_ast(Source)` - Generate Core Erlang AST from source
- `format_result(Term)` - Format an Erlang term as Surreal syntax

### surreal_compiler

- `compile_project(Path)` - Compile an entire Surreal project
- `compile_file(Path)` - Compile a single Surreal file

## License

MIT
