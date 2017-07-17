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

fn adjacent<A: Match, B: Match>(a: &A, b: &B, sentence: &str) -> bool {
    a.byte_range().1 <= b.byte_range().0 &&
    sentence[a.byte_range().1..b.byte_range().0]
        .chars()
        .all(|c| c.is_whitespace())
}

#[derive(Copy,Clone,Debug,PartialEq)]
struct NodeInfo {
    storage_position: usize,
    sentence_position: usize,
}

pub struct Stash<'s, S: StashIndexable + NodePayload> {
    pub sentence: &'s str,
    storage: Vec<ParsedNode<S>>,
    index: HashMap<S::Index, Vec<NodeInfo>>,
}

impl<'s, S: StashIndexable + NodePayload> Stash<'s, S> {

    pub fn new(sentence: &'s str) -> Stash<'s, S> {
        Stash {
            sentence,
            storage: vec![],
            index: HashMap::new(),
        }
    }
    
    pub fn extend(&mut self, nodes: Vec<ParsedNode<S>>) {
        for node in nodes.into_iter() {
            self.push(node);
        }
    }
    pub fn push(&mut self, node: ParsedNode<S>) {
        let node_info = NodeInfo { 
            storage_position: self.storage.len(), 
            sentence_position: node.root_node.byte_range.0, 
        };

        let node_index = node.value.index();

        self.storage.push(node);
        let index_position = self.index
            .get(&node_index)
            .map(|nodes_info| {
                nodes_info.binary_search_by(|info| info.sentence_position.cmp(&node_info.sentence_position))
                    .unwrap_or_else(|e| e)
            })
            .unwrap_or(0);

        self.index
            .entry(node_index)
            .or_insert(vec![])
            .insert(index_position, node_info);
    }

    pub fn nearest_neighbors_from<V, F>(&self, position: usize, predicate: F) -> Vec<ParsedNode<V>> 
        where V: InnerStashIndexable<Index=S::Index> + NodePayload<Payload=S::Payload> + AttemptFrom<S>,
              F: Fn(&V) -> bool,
    {
        self.index.get(&V::index())
            .map(move |nodes| {
                nodes.iter().filter_map(move |node_info| {
                    self.format_node(node_info, &predicate)
                }).collect()
            })
            .unwrap_or(vec![])
    }

    pub fn find_all<V, F>(&self, predicate: F) -> Vec<ParsedNode<V>> 
        where V: InnerStashIndexable<Index=S::Index> + NodePayload<Payload=S::Payload> + AttemptFrom<S>,
              F: Fn(&V) -> bool,
    {
        self.index.get(&V::index())
            .map(move |nodes| {
                nodes.iter().filter_map(move |node_info| {
                    self.format_node(node_info, &predicate)
                }).collect()
            })
            .unwrap_or(vec![])
    }

    fn format_node<V, F>(&self, node_info: &NodeInfo, predicate: &F) -> Option<ParsedNode<V>> 
        where V: InnerStashIndexable<Index=S::Index> + NodePayload<Payload=S::Payload> + AttemptFrom<S>,
              F: Fn(&V) -> bool {
        self.storage.get(node_info.storage_position)
            .as_ref()
            .and_then(|node| {
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
            })
    }
 
    pub fn iter(&self) ->  Iter<ParsedNode<S>> {
        self.storage.iter()
    }
    pub fn into_iter(self) -> IntoIter<ParsedNode<S>> {
        self.storage.into_iter()
    }

    pub fn values<'b>(&'b self) -> &'b Vec<ParsedNode<S>> {
        self.storage.as_ref()
    }

    pub fn len(&self) -> usize {
        self.storage.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::*;
    use smallvec::SmallVec;
    
    #[derive(Copy,Clone,Debug,PartialEq)]
    struct MyPayload;

    #[derive(Copy,Clone,Debug,PartialEq)]
    enum MyValue {
        A(AValue),
        B(BValue),
    }

    impl MyValue {
        fn a(value: usize) -> MyValue {
            MyValue::A(AValue(value))
        }

        fn b(value: usize) -> MyValue {
            MyValue::B(BValue(value))
        }
    }

    #[derive(Copy,Clone,Debug,PartialEq)]
    struct AValue(usize);
    #[derive(Copy,Clone,Debug,PartialEq)]
    struct BValue(usize);

    impl StashIndexable for MyValue {
        type Index = Kind;
        fn index(&self) -> Self::Index {
            match self {
                &MyValue::A(_) => Kind::A,
                &MyValue::B(_) => Kind::B,
            }
        }
    }

    impl AttemptFrom<MyValue> for AValue {
        fn attempt_from(v: MyValue) -> Option<AValue> {
            match v {
                MyValue::A(a) => Some(a),
                MyValue::B(_) => None,
            }
        }
    }

    impl AttemptFrom<MyValue> for BValue {
        fn attempt_from(v: MyValue) -> Option<BValue> {
            match v {
                MyValue::A(_) => None,
                MyValue::B(b) => Some(b),
            }
        }
    }

    impl InnerStashIndexable for AValue {
        type Index = Kind;
        fn index() -> Self::Index {
            Kind::A
        }
    }

    impl InnerStashIndexable for BValue {
        type Index = Kind;
        fn index() -> Self::Index {
            Kind::B
        }
    }

    #[derive(Copy,Clone,Debug,PartialEq, Hash, Eq)]
    enum Kind {
        A,
        B,
    }

    impl NodePayload for MyValue {
        type Payload = MyPayload;
        fn extract_payload(&self) -> Option<Self::Payload> {
            None
        }
    }

    impl NodePayload for AValue {
        type Payload = MyPayload;
        fn extract_payload(&self) -> Option<Self::Payload> {
            None
        }
    }

    impl NodePayload for BValue {
        type Payload = MyPayload;
        fn extract_payload(&self) -> Option<Self::Payload> {
            None
        }
    }

    impl NodeInfo {
        fn new(storage_position: usize, sentence_position: usize) -> NodeInfo {
            NodeInfo {
                storage_position,
                sentence_position,
            }
        }
    }

    fn build_parse_node(range: Range, value: MyValue) -> ParsedNode<MyValue> {
        ParsedNode::new(Sym(1), value, range, None, SmallVec::new())
    }

    #[test]
    fn test_node_indexing() {
        let mut stash = Stash::default();
        let nodes = vec![
            build_parse_node(Range(1, 3), MyValue::a(1)),
            build_parse_node(Range(4, 10), MyValue::b(2)),
            build_parse_node(Range(2, 3), MyValue::a(1)),
            build_parse_node(Range(5,  10),  MyValue::a(3)),
            build_parse_node(Range(2, 10),  MyValue::b(2)),
        ];

        stash.extend(nodes);

        assert_eq!(vec![
                NodeInfo::new(0, 1),
                NodeInfo::new(2, 2),
                NodeInfo::new(3, 5),
            ], stash.index.get(&Kind::A).unwrap().clone());

        assert_eq!(vec![
                NodeInfo::new(4, 2),
                NodeInfo::new(1, 4),
            ], stash.index.get(&Kind::B).unwrap().clone());
    }

    #[test]
    fn test_filter_predicate() {
        let mut stash = Stash::default();
        let nodes = vec![
            build_parse_node(Range(1, 3), MyValue::a(1)),
            build_parse_node(Range(4, 10), MyValue::b(2)),
            build_parse_node(Range(2, 3), MyValue::a(1)),
            build_parse_node(Range(5,  10),  MyValue::a(3)),
            build_parse_node(Range(2, 10),  MyValue::b(2)),
        ];
        stash.extend(nodes);

        let nodes = stash.filter(|value: &AValue| value.0 == 1);
    }
}