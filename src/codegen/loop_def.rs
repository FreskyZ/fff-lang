
// loop operation
// store temp values for loop name and refill addresses

use codegen::CodeID;
use codegen::CodeCollection;

pub struct Loop {
    pub name: Option<String>,
    pub continue_addr: CodeID,
    pub break_addrs: Vec<CodeID>,
}

pub struct LoopCollection {
    loops: Vec<Loop>,
}

impl LoopCollection {
    pub fn new() -> LoopCollection {
        LoopCollection{ 
            loops: Vec::new(),    
        }
    }

    pub fn push_loop(&mut self, name: Option<String>, continue_addr: CodeID) {
        self.loops.push(Loop{ name: name, continue_addr: continue_addr, break_addrs: Vec::new() })
    }
    // Panic on empty contents
    pub fn pop_and_refill(&mut self, refill_adder: CodeID, codes: &mut CodeCollection) {
        let last = self.loops.pop().unwrap();
        for break_addr in last.break_addrs {
            codes.refill_addr(break_addr, refill_adder);
        }
    }

    pub fn get_last_loop_continue_addr(&self) -> Option<CodeID> {
        match self.loops.len() {
            0 => None,
            n => Some(self.loops[n - 1].continue_addr),
        }
    }
    pub fn get_loop_continue_addr(&self, name: &str) -> Option<CodeID> {
        for lp in &self.loops {
            match lp.name {
                Some(ref lp_name) => {
                    if lp_name == name {
                        return Some(lp.continue_addr);
                    }
                }
                _ => (),
            }
        }
        return None;
    }
    pub fn push_last_loop_break_addr(&mut self, break_addr: CodeID) -> Option<()> {
        match self.loops.len() {
            0 => None,
            n => {
                self.loops[n - 1].break_addrs.push(break_addr);
                Some(())
            }
        }
    }
    pub fn push_loop_break_addr(&mut self, name: &str, break_addr: CodeID) -> Option<()> {
        for mut lp in &mut self.loops {
            match lp.name {
                Some(ref lp_name) => {
                    if lp_name == name {
                        lp.break_addrs.push(break_addr);
                        return Some(());
                    }
                }
                _ => (),
            }
        }
        return None;
    } 
}