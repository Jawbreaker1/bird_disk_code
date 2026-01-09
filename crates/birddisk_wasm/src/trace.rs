use birddisk_core::{ast::Program, TraceFrame};
use std::collections::HashMap;

pub(crate) struct TraceTable {
    pub(crate) frames: Vec<TraceFrame>,
    pub(crate) ids: HashMap<String, i32>,
}

pub(crate) fn build_trace_table(program: &Program) -> TraceTable {
    let mut frames = Vec::new();
    let mut ids = HashMap::new();
    let mut insert = |name: String, span| {
        let id = frames.len() as i32;
        frames.push(TraceFrame { function: name.clone(), span });
        ids.insert(name, id);
    };
    for func in &program.functions {
        insert(func.name.clone(), func.span);
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            insert(name, method.span);
        }
    }
    TraceTable { frames, ids }
}
