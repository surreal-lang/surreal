//! Bytecode instructions for the VM.

use crate::Pid;

/// Bytecode instructions
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Process is done
    End,

    /// Do some work (costs `amount` reductions)
    Work { amount: u32 },

    /// Spawn a new process running `code`, store child PID in register
    Spawn { code: Vec<Instruction>, dest: Register },

    /// Spawn a new process and atomically link to it
    SpawnLink { code: Vec<Instruction>, dest: Register },

    /// Send a message to a process
    Send { to: Source, msg: String },

    /// Receive a message matching a pattern, block if none available
    /// For now, just receives any user message into a register
    Receive { dest: Register },

    /// Receive with timeout (in reductions). If no message arrives
    /// before timeout expires, receives "TIMEOUT" instead.
    ReceiveTimeout { dest: Register, timeout: u32 },

    /// Link to another process (bidirectional crash notification)
    Link { target: Source },

    /// Monitor another process (one-way crash notification)
    Monitor { target: Source },

    /// Register current process with a name
    Register { name: String },

    /// Unregister a name
    Unregister { name: String },

    /// Look up a registered name, store PID (or None) in register
    WhereIs { name: String, dest: Register },

    /// Print a value (for debugging)
    Print { source: Source },

    /// Crash the process (for testing links)
    Crash,
}

/// Where to read a value from
#[derive(Debug, Clone)]
pub enum Source {
    /// A register
    Reg(Register),
    /// The process's own PID
    Self_,
    /// The parent process's PID (if any)
    Parent,
    /// A literal PID
    Pid(Pid),
    /// A named process from the registry
    Named(String),
}

/// Register index (processes have a small set of registers)
#[derive(Debug, Clone, Copy)]
pub struct Register(pub u8);
