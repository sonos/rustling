#[macro_use]
extern crate error_chain;
pub extern crate regex;
extern crate smallvec;
extern crate string_interner;
#[macro_use]
extern crate serde_derive;

use string_interner::StringInterner;

use smallvec::SmallVec;
use std::{rc, cell};
use std::fmt::Debug;
use std::collections::HashMap;

pub mod pattern;
pub mod rule;
mod builder;
mod range;
mod helpers;

use rule::Rule;
use pattern::Pattern;
pub use range::Range;
pub use rule::rule_errors::*;
pub use builder::RuleSetBuilder;
pub use helpers::BoundariesChecker;

use errors::*;
pub mod errors {
    error_chain! {
        types {
            CoreError, CoreErrorKind, CoreResultExit, CoreResult;
        }
        foreign_links {
            Regex(::regex::Error);
        }
        errors {
            ProductionRuleError(t: String)
        }
    }
}

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

#[derive(Debug, Clone)]
pub struct PreprocessedInput {
    pub original_input: String,
    pub preprocessed_input: String,
    byte_mapping: HashMap<usize, usize>
}

impl PreprocessedInput {
    pub fn new(original_input: String, preprocessed_input: String, byte_mapping: HashMap<usize, usize>) -> PreprocessedInput {
        PreprocessedInput {
            original_input,
            preprocessed_input,
            byte_mapping,
        }
    }

    pub fn map_byte_range(&self, range: &Range) -> CoreResult<Range> {
        let start = self.map_byte(range.0)?;
        let end = self.map_byte(range.1)?;
        let mapped_range = Range(start, end);
        if mapped_range.0 > mapped_range.1 { 
            Err(format!("Range {:?} in the preprocessed input: {:?} cannot be mapped with Range: {:?} in the orginal input {:?}", 
                range, 
                self.preprocessed_input, 
                mapped_range,  
                self.original_input))?
        } else {
            Ok(mapped_range)
        }
    }

    fn map_byte(&self, byte: usize) -> CoreResult<usize> {
        println!("-------------------------------");
        println!("{:?}", self.byte_mapping);
        println!("{:?}", self.original_input.len());
        println!("{:?}", self.preprocessed_input.len());

        match self.byte_mapping.get(&byte) {
            None => Err(format!("Byte {:?} not found in the mapping, original: {:?} and preprocessed: {:?}", 
                        byte, 
                        self.original_input, 
                        self.preprocessed_input))?,
            Some(mapped_byte) => {
                println!("{:?}", mapped_byte);
                if *mapped_byte <= self.original_input.len() {
                    Ok(*mapped_byte)
                } else {
                    Err(format!("Mapped Byte {:?} not found in the original input, original: {:?} and preprocessed: {:?}", 
                        byte, 
                        self.original_input, 
                        self.preprocessed_input))?
                }
            }
        }
    }   
}

pub trait Preprocessor {
    fn run(&self, input: &str) -> PreprocessedInput;
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

pub type Stash<V> = Vec<ParsedNode<V>>;

pub struct RuleSet<StashValue: NodePayload, P: Preprocessor> {
    symbols: SymbolTable,
    rules: Vec<Box<Rule<StashValue>>>,
    preprocessor: P,
}

impl<StashValue: NodePayload+Debug, P: Preprocessor> RuleSet<StashValue, P> {
    fn apply_once(&self, stash: &mut Stash<StashValue>, input: &PreprocessedInput) -> CoreResult<()> {
        let mut produced_nodes = vec![];
        for rule in &self.rules {
            produced_nodes.extend(rule.apply(stash, input)?);
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    pub fn apply_all(&self, sentence: &str) -> CoreResult<Stash<StashValue>> {
        let iterations_max = 10;
        let max_stash_size = 600;
        let input = self.preprocessor.run(sentence);
        let mut stash = vec![];
        let mut previous_stash_size = 0;
        for _ in 0..iterations_max {
            self.apply_once(&mut stash, &input)?;
            if stash.len() <= previous_stash_size || stash.len() > max_stash_size {
                break;
            }
            previous_stash_size = stash.len();
        }
        Ok(stash.into_iter().filter(|pn| BoundariesChecker::SperatedAlphanumericWord.check(&input.original_input, pn.root_node.byte_range)).collect())
    }

    pub fn resolve_sym(&self, sym:&Sym) -> Option<&str> {
        self.symbols.0.resolve(*sym)
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
