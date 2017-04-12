use std::fmt::Debug;

use regex::Regex;

use core::rule::*;
use core::*;

#[derive(Debug)]
pub struct RegexPattern<V:Value> {
    pub regex: Regex,
    pub _phantom: ::std::marker::PhantomData<V>
}

impl<V:Value> RegexPattern<V> {
    pub fn new(regex:Regex) -> RegexPattern<V> {
        RegexPattern {
            regex: regex,
            _phantom: ::std::marker::PhantomData
        }
    }
}

impl<V:Value> Pattern<V> for RegexPattern<V> {
    fn predicate(&self, _stash: &Stash<V>, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match<V>> {
        self.regex
            .captures_iter(&sentence[start..])
            .map(|cap| {
                Match::Text(cap.iter()
                                .map(|m| {
                                         // FIXME
                                         let m = m.unwrap();
                                         Range {
                                             start: start + m.start(),
                                             end: start + m.end(),
                                         }
                                     })
                                .collect(),
                                rule_name)
            })
            .collect()
    }
}

pub trait NodePredicate<V:Value>: Debug {
    fn predicate(&self, node: &ParsedNode<V>) -> bool;
}

#[derive(Debug)]
pub struct NodePattern<V:Value> {
    pub predicate: Box<NodePredicate<V>>,
}


impl<V:Value> Pattern<V> for NodePattern<V> {

    fn predicate(&self, stash: &Stash<V>, _sentence: &str, start: usize, _rule_name: &'static str) -> Vec<Match<V>> {
        stash
            .iter()
            .filter(|node| self.predicate.predicate(node) && node.root_node.range.start >= start)
            .map(|node| Match::ParsedNode(node.clone()))
            .collect()
    }
}
