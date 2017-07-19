use ::{ParsedNode, NodePayload, AttemptFrom};
use std::hash::Hash;
use std::collections::HashMap;
use std::slice::Iter;
use std::vec::IntoIter;
use pattern::Match;

pub trait StashIndexable {
    type Index: Hash+Eq;
    fn index(&self) -> Self::Index;
}

pub trait InnerStashIndexable {
    type Index: Hash+Eq;
    fn index() -> Self::Index;
}

pub struct Stash<S: StashIndexable + NodePayload> {
    values: Vec<ParsedNode<S>>,
    index: HashMap<S::Index, Vec<usize>>,
}

impl<S: StashIndexable + NodePayload> Default for Stash<S> {
    fn default() -> Stash<S> {
        Stash {
            values: vec![],
            index: HashMap::new(),
        }
    }
}

impl<S: StashIndexable + NodePayload> Stash<S>{
    pub fn extend(&mut self, nodes: Vec<ParsedNode<S>>) {
        for node in nodes.into_iter() {
            self.push(node);
        }
    }
    pub fn push(&mut self, node: ParsedNode<S>) {
        let node_position = self.values.len();
        let node_index = node.value.index();
        self.values.push(node);
        self.index
            .entry(node_index)
            .or_insert(vec![])
            .push(node_position);
    }

    pub fn filter<V, F>(&self, predicate: F) -> Vec<ParsedNode<V>> 
        where V: InnerStashIndexable<Index=S::Index> + NodePayload<Payload=S::Payload> + AttemptFrom<S>,
              F: Fn(&V) -> bool,
    {
        self.index.get(&V::index())
            .map(|nodes| {
                nodes.iter().filter_map(|position| {
                    let ref node = self.values[*position];
                    if let Some(v) = V::attempt_from(node.value.clone()) {
                               if (predicate)(&v) {
                                   Some(ParsedNode::new(node.root_node.rule_sym,
                                                        v,
                                                        node.byte_range(),
                                                        node.root_node.payload.clone(),
                                                        node.root_node.children.clone()))
                               } else {
                                   None
                               }
                    } else {
                        None
                    }
                }).collect()
            })
            .unwrap_or(vec![])
    }

    pub fn iter(&self) ->  Iter<ParsedNode<S>> {
        self.values.iter()
    }
    pub fn into_iter(self) -> IntoIter<ParsedNode<S>> {
        self.values.into_iter()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    #[cfg(test)]
    pub fn values<'a>(&'a self) -> &'a Vec<ParsedNode<S>> {
        self.values.as_ref()
    }
}