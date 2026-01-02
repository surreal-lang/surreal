//! Process identifier type.

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Process identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub struct Pid(pub u64);

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
impl Pid {
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    pub fn id(&self) -> u64 {
        self.0
    }
}
