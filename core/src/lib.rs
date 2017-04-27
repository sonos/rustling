#[macro_use]
extern crate error_chain;
pub extern crate regex;
extern crate smallvec;

use smallvec::SmallVec;
use std::rc;

pub mod pattern;
pub mod rule;


use rule::Rule;
pub use pattern::Range;

pub use rule::rule_errors::*;

use errors::*;
pub mod errors {
    error_chain! {
        types {
            CoreError, CoreErrorKind, CoreResultExit, CoreResult;
        }
        errors {
            ProductionRuleError(t: String)
        }
    }
}

pub trait AttemptFrom<V>: Sized {
    fn attempt_from(v: V) -> Option<Self>;
}

pub trait AttemptTo<T>: Sized {
    fn attempt_to(&self) -> Option<T>;
}

impl<S,T> AttemptTo<T> for S where S:Clone, T: AttemptFrom<S> {
    fn attempt_to(&self) -> Option<T> {
        T::attempt_from(self.clone())
    }
}

pub type ChildrenNodes = SmallVec<[rc::Rc<Node>;2]>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Node {
    pub rule_name: &'static str,
    pub range: Range,
    pub children: ChildrenNodes,
}

impl Node {
    fn new(name: &'static str, range: Range, children: ChildrenNodes) -> rc::Rc<Node> {
        rc::Rc::new(Node {
            rule_name: name,
            range: range,
            children: children,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedNode<V: Clone> {
    pub root_node: rc::Rc<Node>,
    pub value: V,
}

impl<V: Clone> ParsedNode<V> {
    fn new(name: &'static str, v: V, r: Range, children: ChildrenNodes) -> ParsedNode<V> {
        ParsedNode {
            root_node: Node::new(name, r, children),
            value: v,
        }
    }
}

pub type Stash<V> = Vec<ParsedNode<V>>;

pub struct RuleSet<StashValue: Clone>(pub Vec<Box<Rule<StashValue>>>);

impl<V: Clone> RuleSet<V> {
    fn apply_once(&self, stash: &mut Stash<V>, sentence: &str) -> CoreResult<()> {
        let mut produced_nodes = vec!();
        for rule in &self.0 {
            produced_nodes.extend(rule.apply(stash, sentence)?);
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    pub fn apply_all(&self, sentence: &str) -> CoreResult<Stash<V>> {
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
        Ok(stash)
    }
}

