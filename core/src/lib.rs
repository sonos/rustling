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

pub mod pattern;
pub mod rule;
mod builder;
mod range;

use rule::Rule;
use pattern::Pattern;
pub use range::Range;
pub use rule::rule_errors::*;
pub use builder::RuleSetBuilder;

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

pub struct RuleSet<StashValue: NodePayload> {
    symbols: SymbolTable,
    rules: Vec<Box<Rule<StashValue>>>,
}

impl<StashValue: NodePayload> RuleSet<StashValue> {
    fn apply_once(&self, stash: &mut Stash<StashValue>, sentence: &str) -> CoreResult<()> {
        let mut produced_nodes = vec![];
        for rule in &self.rules {
            produced_nodes.extend(rule.apply(stash, sentence)?);
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    pub fn apply_all(&self, sentence: &str) -> CoreResult<Stash<StashValue>> {
        let iterations_max = 10;
        let max_stash_size = 600;
        let mut stash = vec![];
        let mut previous_stash_size = 0;
        for _ in 0..iterations_max {
            self.apply_once(&mut stash, sentence)?;
            if stash.len() <= previous_stash_size && stash.len() > max_stash_size {
                break;
            }
            previous_stash_size = stash.len();
        }
        Ok(stash.into_iter().filter(|pn| valid_boundaries(sentence, pn.root_node.byte_range, &alphanumeric_class)).collect())
    }

    pub fn resolve_sym(&self, sym:&Sym) -> Option<&str> {
        self.symbols.0.resolve(*sym)
    }
}

fn alphanumeric_class(c: char) -> char {
    if c.is_alphanumeric() { 'A' } else { c }
}

fn detailed_class(c: char) -> char {
    if c.is_uppercase() {
        'u'
    } else if c.is_lowercase() {
        'l'
    } else if c.is_digit(10) {
        'd'
    } else {
        c
    }
}

fn valid_boundaries<CharClass>(sentence: &str, range: Range, char_class: &CharClass) -> bool
    where CharClass: Fn(char) -> char
{
    let first_mine = sentence[range.0..range.1]
        .chars()
        .next()
        .map(char_class); // Some(c)
    let last_mine = sentence[range.0..range.1]
        .chars()
        .next_back()
        .map(char_class); //Some(c)
    let last_before = sentence[..range.0].chars().next_back().map(char_class); // Option(c)
    let first_after = sentence[range.1..].chars().next().map(char_class); // Option(c)

    first_mine != last_before && last_mine != first_after
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_boundaries() {
        let an = |c: char| if c.is_alphanumeric() { 'A' } else { c };
        assert_eq!(true, valid_boundaries("abc def ret", Range(4, 7), &an)); // "def"
        assert_eq!(false, valid_boundaries("abc def ret", Range(2, 8), &an)); // "c def r"
        assert_eq!(false,
                   valid_boundaries("abc def123 ret", Range(4, 7), &an)); // "def"
        assert_eq!(true, valid_boundaries("def123 ret", Range(0, 6), &an)); // "def123"
        assert_eq!(false, valid_boundaries("def123 ret", Range(0, 3), &an)); // "def"
        assert_eq!(true, valid_boundaries("ret def", Range(4, 7), &an)); // "def"
        assert_eq!(false, valid_boundaries("ret 123def", Range(7, 10), &an)); // "def"
        assert_eq!(false, valid_boundaries("aéc def ret", Range(3, 9), &an)); // "c def r"
        assert_eq!(false, valid_boundaries("aec def rét", Range(2, 8), &an)); // "c def r"
        assert_eq!(false, valid_boundaries("aec déf ret", Range(2, 9), &an)); // "c déf r"
        assert_eq!(false, valid_boundaries("aeç def ret", Range(2, 9), &an)); // "ç def r"
        assert_eq!(true, valid_boundaries("aeç def ret", Range(4, 8), &an)); // " def "
    }
}
