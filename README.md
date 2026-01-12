# Dream

A programming language with Rust-like syntax and Erlang-style concurrency.

Dream combines Rust's familiar syntax with Erlang's battle-tested concurrency model - processes, message passing, and pattern matching. It compiles to BEAM bytecode for full Erlang/OTP ecosystem integration.

## Quick Start

```bash
# Create a new project
dream new my_app
cd my_app

# Build the project
dream build

# Run the project
dream run

# Run tests
dream test

# Start the REPL
dream shell
```

## Compilation

```
Source → Lexer → Parser → AST → Core Erlang → BEAM
```

Dream compiles to Core Erlang, which is then compiled to BEAM bytecode. This gives you full access to the Erlang/OTP ecosystem, including OTP behaviors, Hex packages, and interop with Erlang and Elixir libraries.

## Language Features

### Concurrency

```rust
use process::send;

pub fn main() {
    let pid = spawn(worker());
    send(pid, (:hello, self()));

    receive {
        (:reply, msg) => println(msg)
    }
}

fn worker() {
    receive {
        (:hello, sender) => send(sender, (:reply, "Hello from worker!"))
    }
}
```

- **Processes**: lightweight, isolated units of execution
- **Message Passing**: async send, receive with timeout, selective receive
- **Links**: bidirectional crash notification between processes
- **Monitors**: one-way crash notification (DOWN messages)
- **Process Registry**: register/unregister/whereis for named processes

### Data Types

- Integers (arbitrary precision)
- Floats
- Atoms (`:ok`, `:error`)
- Tuples and Lists
- Strings
- PIDs (process identifiers)
- Binaries/Bitstrings
- Maps
- `Result<T, E>` and `Option<T>`

### Pattern Matching

```rust
fn describe(value: any) -> string {
    match value {
        0 => "zero",
        n if n > 0 => "positive",
        n if n < 0 => "negative",
        (:ok, result) => "success",
        (:error, _) => "failure",
        [head, ..tail] => "non-empty list",
        [] => "empty list",
        _ => "unknown"
    }
}
```

### Error Handling

```rust
fn might_fail(x: int) -> Result<int, string> {
    if x < 0 {
        Err("negative number")
    } else {
        Ok(x * 2)
    }
}

fn chain_operations(x: int) -> Result<int, string> {
    let a = might_fail(x)?;    // Returns Err early if x < 0
    let b = might_fail(a)?;    // Propagates error
    Ok(b)
}
```

### Structs and Enums

```rust
struct User {
    id: int,
    name: string,
    email: Option<string>,
}

impl User {
    pub fn new(id: int, name: string) -> User {
        User { id, name, email: None }
    }
}

enum Status {
    Active,
    Inactive,
    Pending { reason: string },
}
```

### Traits

```rust
trait Display {
    fn display(self) -> string;
}

impl Display for User {
    pub fn display(self) -> string {
        self.name
    }
}
```

### Attributes

```rust
#[test]
fn test_addition() {
    assert_eq(1 + 1, 2)
}

#[cfg(feature = "json")]
pub fn parse_json(s: string) -> any {
    :jason::decode(s)
}

#[cfg(test)]
mod tests {
    // Only compiled during testing
}
```

### OTP Integration

Implement OTP behaviors using traits:

```rust
use application::Application;

impl Application {
    pub fn start(_type: atom, _args: any) -> Result<pid, any> {
        // Start your application
        let pid = spawn(my_server());
        Ok(pid)
    }

    pub fn stop(_state: pid) -> atom {
        :ok
    }
}
```

### Pipe Operator

```rust
let result = data
    |> transform()
    |> filter(predicate)
    |> map(func);
```

### Erlang Interop

Call Erlang/Elixir functions directly:

```rust
// Call Erlang module
let result = :lists::reverse([1, 2, 3]);

// Call Elixir module
let json = :"Elixir.Jason"::encode(data);
```

## Project Structure

A Dream project looks like this:

```
my_app/
├── dream.toml           # Project configuration
├── src/
│   ├── lib.dream        # Root module
│   ├── app.dream        # Application entry point
│   └── handlers/
│       ├── mod.dream    # Submodule declarations
│       └── api.dream    # Handler implementation
└── _build/              # Build artifacts
```

### dream.toml

```toml
[package]
name = "my_app"
version = "0.1.0"

[application]
mod = "my_app::app"      # OTP application module

[dependencies]
cowboy = "2.12.0"        # Hex packages
jason = "1.4.4"

[features]
json = []
full = ["json"]
```

### Module System

Dream uses a Rust-like module system:

```rust
// src/lib.dream - root module
mod app;           // Loads src/app.dream
mod handlers;      // Loads src/handlers/mod.dream

pub fn version() -> string {
    "1.0.0"
}
```

```rust
// src/handlers/mod.dream
mod api;           // Loads src/handlers/api.dream
mod websocket;
```

## CLI Commands

| Command | Description |
|---------|-------------|
| `dream new <name>` | Create a new project |
| `dream build` | Build the project |
| `dream run` | Build and run |
| `dream test` | Run tests |
| `dream test "pattern"` | Run tests matching pattern |
| `dream shell` | Interactive REPL |
| `dream deps get` | Fetch dependencies |
| `dream deps update` | Update dependencies |
| `dream bindgen` | Generate type stubs from Erlang |

### Build Options

```bash
dream build --features json      # Enable features
dream test --features json       # Test with features
```

## Building from Source

```bash
# Clone the repository
git clone https://github.com/scrogson/dream
cd dream

# Build the compiler
cargo build --release

# Run tests
cargo test

# Install locally
cargo install --path .
```

## Examples

See the `examples/` directory for complete examples:

- `http_api/` - JSON API server using Cowboy
- `concurrency/` - Process spawning and message passing
- `genserver/` - OTP GenServer implementation

## Related Projects

- [tree-sitter-dream](https://github.com/scrogson/tree-sitter-dream) - Tree-sitter grammar for editor support

## License

MIT
