//! Process state and status.

use std::collections::VecDeque;

use crate::{Instruction, Message, Pid, Value};

/// Process state
#[derive(Debug)]
pub struct Process {
    pub pid: Pid,
    pub parent: Option<Pid>,
    pub code: Vec<Instruction>,
    pub pc: usize,
    pub registers: [Value; 8],
    pub mailbox: VecDeque<Message>,
    pub links: Vec<Pid>,
    /// Processes that are monitoring this one (one-way)
    pub monitored_by: Vec<Pid>,
    pub status: ProcessStatus,
    /// Remaining timeout (in reductions) when waiting for a message
    pub timeout: Option<u32>,
}

/// Process execution status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStatus {
    /// Ready to run
    Ready,
    /// Waiting for a message
    Waiting,
    /// Finished normally
    Done,
    /// Crashed
    Crashed,
}

impl Process {
    pub fn new(pid: Pid, parent: Option<Pid>, code: Vec<Instruction>) -> Self {
        Self {
            pid,
            parent,
            code,
            pc: 0,
            registers: std::array::from_fn(|_| Value::None),
            mailbox: VecDeque::new(),
            links: Vec::new(),
            monitored_by: Vec::new(),
            status: ProcessStatus::Ready,
            timeout: None,
        }
    }
}
