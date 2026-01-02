//! Message types for inter-process communication.

use crate::Pid;

/// Messages sent between processes
#[derive(Debug, Clone)]
pub enum Message {
    /// User-level message
    User(String),
    /// System message (e.g., crash notification from linked process)
    System(SystemMsg),
}

/// System-level messages
#[derive(Debug, Clone)]
pub enum SystemMsg {
    /// A linked process exited normally
    Exit(Pid),
    /// A linked process crashed
    Crash(Pid),
    /// A monitored process exited (with reason)
    Down(Pid, DownReason),
}

/// Reason for a monitored process going down
#[derive(Debug, Clone)]
pub enum DownReason {
    Normal,
    Crashed,
}
