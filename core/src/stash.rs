use ::{ParsedNode, NodePayload, AttemptFrom};
use std::hash::Hash;
use std::collections::HashMap;
use pattern::Match;

trait StashIndexable {
    type Index: Hash+Eq;
    fn index(&self) -> Self::Index;
}

trait ValueIndexable {
    type Index: Hash+Eq;
    fn index() -> Self::Index;
}

struct Stash<S: StashIndexable + NodePayload> {
    index: HashMap<S::Index, Vec<ParsedNode<S>>>
}

impl<S: StashIndexable + NodePayload> Stash<S>{
    fn push(&mut self, node: ParsedNode<S>) {
        self.index
            .entry(node.value.index())
            .or_insert(vec![])
            .push(node);
    }

    fn filter<V, F>(&mut self, predicate: F) -> Vec<ParsedNode<V>> 
        where V: ValueIndexable<Index=S::Index> + NodePayload<Payload=S::Payload> + AttemptFrom<S>,
              F: Fn(&V) -> bool,
    {
        self.index.get(&V::index())
            .map(|nodes| {
                nodes.iter().filter_map(|it| {
                    if let Some(v) = V::attempt_from(it.value.clone()) {
                               if (predicate)(&v) {
                                   Some(ParsedNode::new(it.root_node.rule_sym,
                                                        v,
                                                        it.byte_range(),
                                                        it.root_node.payload.clone(),
                                                        it.root_node.children.clone()))
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
}