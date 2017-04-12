use std::fmt::Debug;

use regex::Regex;

use core::rule::*;
use core::*;

#[derive(Debug)]
pub struct RegexPattern(pub Regex);

impl Pattern for RegexPattern {
    fn predicate(&self, _stash: &Stash, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match> {
        self.0
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

pub trait NodePredicate: Debug {
    fn predicate(&self, node: &ParsedNode) -> bool;
}

#[derive(Debug)]
pub struct NodePattern {
    pub predicate: Box<NodePredicate>,
}


impl Pattern for NodePattern {

    fn predicate(&self, stash: &Stash, _sentence: &str, start: usize, _rule_name: &'static str) -> Vec<Match> {
        stash
            .iter()
            .filter(|node| self.predicate.predicate(node) && node.root_node.range.start >= start)
            .map(|node| Match::ParsedNode(node.clone()))
            .collect()
    }
}
