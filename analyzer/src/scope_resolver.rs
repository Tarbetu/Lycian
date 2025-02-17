mod scope;
mod scope_hierarchy;
mod symbol;
use crate::ResolutionResult;
use parser::{Class, Name, NameIndex};
pub use scope::*;
pub use scope_hierarchy::*;
use symbol::{Symbol, SymbolId, SymbolType};

use ahash::{AHashMap, HashMap, HashMapExt};
use bimap::BiHashMap;
use rayon::prelude::*;
use std::sync::{
    atomic::AtomicUsize,
    mpsc::{channel, Sender},
};

enum SymbolMessage {
    NewSymbol(SymbolId, Symbol),
    Finish,
}

pub struct ScopeResolver<'a> {
    // symbol_receiver: Receiver<SymbolMessage>,
    // symbol_sender: Sender<SymbolMessage>,
    names: &'a BiHashMap<NameIndex, Name>,
    classes: &'a AHashMap<NameIndex, Class>,
    last_id: AtomicUsize,
}

impl<'a> ScopeResolver<'a> {
    pub fn new(
        names: &'a BiHashMap<NameIndex, Name>,
        classes: &'a AHashMap<NameIndex, Class>,
    ) -> ScopeResolver<'a> {
        ScopeResolver {
            names,
            // symbol_receiver,
            // symbol_sender,
            classes,
            last_id: AtomicUsize::new(0),
        }
    }

    pub fn resolve(self) -> ResolutionResult<ScopeHierarchy> {
        let (symbol_sender, symbol_receiver) = channel();
        let classes: HashMap<SymbolId, Scope> = self
            .classes
            .par_iter()
            .map_with(symbol_sender.clone(), |symbol_sender, (index, class)| {
                let name = self.names.get_by_left(&index).unwrap();
                let symbol = Symbol::from_name(name, SymbolType::Class);
                let symbol_id = self.push_symbol(symbol_sender.clone(), symbol);
                (
                    symbol_id,
                    self.resolve_class(symbol_id, symbol_sender.clone(), &class),
                )
            })
            .collect();
        symbol_sender.send(SymbolMessage::Finish).unwrap();
        let mut symbol_table = SymbolTable::with_capacity(classes.len() * 10);
        // Check that if necessary to check for errors
        while let Ok(msg) = symbol_receiver.recv() {
            match msg {
                SymbolMessage::NewSymbol(symbol_id, symbol) => {
                    symbol_table.insert(symbol_id, symbol);
                }
                SymbolMessage::Finish => break,
            }
        }
        Ok(ScopeHierarchy {
            symbol_table,
            classes,
        })
    }

    fn resolve_class(
        &self,
        itself: SymbolId,
        symbol_sender: Sender<SymbolMessage>,
        class: &Class,
    ) -> Scope {
        let scopes: HashMap<SymbolId, Scope> = class
            .methods
            .par_iter()
            .map_with(
                symbol_sender.clone(),
                |symbol_sender, (index, functions)| {
                    let name = self.names.get_by_left(index).unwrap();
                    let symbol = Symbol::from_name(name, SymbolType::Function);
                    (
                        self.push_symbol(symbol_sender.clone(), symbol),
                        self.resolve_method(symbol_sender.clone(), functions),
                    )
                },
            )
            .collect();
        Scope {
            itself,
            // Classes does not have any locals
            locals: vec![],
            scopes,
        }
    }

    fn resolve_method(
        &self,
        symbol_sender: Sender<SymbolMessage>,
        overloads: &[parser::Function],
    ) -> Scope {
        unimplemented!()
    }

    fn push_symbol(&self, symbol_sender: Sender<SymbolMessage>, symbol: Symbol) -> SymbolId {
        let id = SymbolId(
            self.last_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        );
        symbol_sender
            .send(SymbolMessage::NewSymbol(id, symbol))
            .unwrap();
        id
    }
}
