#[macro_use]
extern crate failure;
pub extern crate regex;
extern crate smallvec;
extern crate string_interner;
#[macro_use]
extern crate serde_derive;

use string_interner::StringInterner;

use smallvec::SmallVec;
use std::{rc, cell};
use std::fmt::Debug;
use std::collections::HashSet;

pub mod pattern;
pub mod rule;
mod builder;
mod range;
mod helpers;
mod stash;

use stash::Stash;
use rule::Rule;
use rule::TerminalRule;
use pattern::Pattern;
use pattern::TerminalPattern;
pub use range::Range;
pub use rule::{RuleResult, RuleError};
pub use builder::RuleSetBuilder;
pub use helpers::BoundariesChecker;
pub use stash::{StashIndexable, InnerStashIndexable};

pub type CoreResult<T> = Result<T, ::failure::Error>;


pub trait AttemptFrom<V>: Sized {
    fn attempt_from(v: V) -> Option<Self>;
}

pub trait AttemptInto<T>: Sized {
    fn attempt_into(self) -> Option<T>;
}

impl<S, T> AttemptInto<T> for S
    where S: Clone,
          T: AttemptFrom<S>
{
    fn attempt_into(self) -> Option<T> {
        T::attempt_from(self)
    }
}

pub trait NodePayload: Clone {
    type Payload: Clone + PartialEq + Debug;
    fn extract_payload(&self) -> Option<Self::Payload>;
}

pub type ChildrenNodes<Payload> = SmallVec<[rc::Rc<Node<Payload>>; 2]>;

#[derive(Copy,Ord,Eq,Clone,PartialEq,PartialOrd,Debug,Hash,Serialize,Deserialize)]
pub struct Sym(usize);
impl string_interner::NonNegative for Sym {}
impl From<usize> for Sym {
    fn from(it: usize) -> Sym {
        Sym(it)
    }
}
impl From<Sym> for usize {
    fn from(it: Sym) -> usize {
        it.0
    }
}

pub struct SymbolTable(StringInterner<Sym>);

impl Default for SymbolTable {
    fn default() -> SymbolTable {
        SymbolTable(string_interner::StringInterner::new())
    }
}

impl SymbolTable {
    pub fn sym<T>(&mut self, val: T) -> Sym
        where T: Into<String> + AsRef<str>
    {
        self.0.get_or_intern(val)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum ParsingStatus {
    Continue,
    Exit,
}

impl ParsingStatus {
    pub fn is_exit(&self) -> bool {
        match self {
            &ParsingStatus::Exit => true,
            _ => false
        }
    }

    pub fn is_continue(&self) -> bool {
        match self {
            &ParsingStatus::Continue => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Node<Payload: Clone> {
    pub rule_sym: Sym,
    pub byte_range: Range,
    pub payload: Option<Payload>,
    pub children: ChildrenNodes<Payload>,
}

impl<Payload: Clone>  Node<Payload> {
    fn new(sym: Sym, byte_range: Range, payload: Option<Payload>, children: ChildrenNodes<Payload>) -> rc::Rc<Node<Payload>> {
        rc::Rc::new(Node {
                        rule_sym: sym,
                        byte_range: byte_range,
                        payload: payload,
                        children: children,
                    })
    }

    pub fn height(&self) -> usize {
        1 + self.children.iter().map(|c| c.height()).max().unwrap_or(0)
    }

    pub fn num_nodes(&self) -> usize {
        let num_children: usize = self.children.iter().map(|c| c.num_nodes()).sum();
        num_children + 1
    }

    pub fn all_syms<'a>(&'a self) -> HashSet<&'a Sym> {
        let mut hash_set = HashSet::new();
        hash_set.insert(&self.rule_sym);
        for child in self.children.iter() {
            for sym in child.all_syms().into_iter() {
                hash_set.insert(sym);
            }
        }
        hash_set
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedNode<V: NodePayload> {
    pub root_node: rc::Rc<Node<V::Payload>>,
    pub value: V,
}

impl<V: NodePayload> ParsedNode<V> {
    fn new(sym: Sym, v: V, r: Range, payload: Option<V::Payload>, children: ChildrenNodes<V::Payload>) -> ParsedNode<V> {
        ParsedNode {
            root_node: Node::new(sym, r, payload, children),
            value: v,
        }
    }
}

pub struct RuleSet<StashValue: NodePayload+StashIndexable> {
    symbols: SymbolTable,
    composition_rules: Vec<Box<Rule<StashValue>>>,
    terminal_rules: Vec<Box<TerminalRule<StashValue>>>,
    match_boundaries: BoundariesChecker,
}

impl<StashValue: NodePayload+StashIndexable> RuleSet<StashValue> {

    fn apply_terminal_rules(&self, stash: &mut Stash<StashValue>, sentence: &str) -> CoreResult<()> {
        let mut produced_nodes = vec![];
        for rule in &self.terminal_rules {
            produced_nodes.extend(rule.apply(stash, sentence)?.nodes);
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    fn apply_composition_rules(&self, stash: &mut Stash<StashValue>, sentence: &str, rules_mask_status: &mut Vec<ParsingStatus>) -> CoreResult<()> {
        let mut produced_nodes = vec![];
        for (idx, rule) in self.composition_rules.iter().enumerate() {
            if rules_mask_status[idx].is_continue() {
                let output = rule.apply(stash, sentence)?;
                rules_mask_status[idx] = output.status;
                produced_nodes.extend(output.nodes);
            }
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    pub fn apply_all(&self, sentence: &str) -> CoreResult<Vec<ParsedNode<StashValue>>> {
        let iterations_max = 10;
        let max_stash_size = 600;
        let mut stash = Stash::default();
        
        self.apply_terminal_rules(&mut stash, sentence)?;
        let mut previous_stash_size = stash.len();

        let mut rules_mask_status = vec!(ParsingStatus::Continue; self.composition_rules.len());
        
        for _ in 0..iterations_max {
            self.apply_composition_rules(&mut stash, sentence, &mut rules_mask_status)?;
            if stash.len() <= previous_stash_size || stash.len() > max_stash_size {
                break;
            }
            previous_stash_size = stash.len();
        }
        Ok(stash.into_iter().filter(|pn| self.match_boundaries.check(sentence, pn.root_node.byte_range)).collect())
    }

    pub fn resolve_sym(&self, sym:&Sym) -> Option<&str> {
        self.symbols.0.resolve(*sym)
    }

    pub fn all_syms(&self) -> Vec<Sym> {
        self.symbols.0.iter().map(|s| s.0).collect()
    }

    pub fn rules_syms(&self) -> Vec<Sym> {
        self.composition_rules.iter().map(|r| r.rule_sym())
            .chain(self.terminal_rules.iter().map(|r| r.rule_sym()))
            .collect()
    }
}

#[derive(Copy,Clone, Debug, PartialEq)]
pub struct SendSyncPhantomData<T>(::std::marker::PhantomData<T>);
unsafe impl<T> Send for SendSyncPhantomData<T> {}
unsafe impl<T> Sync for SendSyncPhantomData<T> {}
impl<T> SendSyncPhantomData<T> {
    pub fn new() -> SendSyncPhantomData<T> {
        SendSyncPhantomData(::std::marker::PhantomData)
    }
}