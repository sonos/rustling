use std::fmt::Debug;
use std::cmp::Eq;
use std::hash::Hash;

use fnv::FnvHashMap;
use fnv::FnvHashSet;

use {Classifier, Feature, FeatureExtractor, Model, Node, RuleId, RuleSet, Truth,
     Value, StashIndexable, ParsedNode};
use RustlingResult;

#[derive(Debug)]
pub struct Example<V: Value> {
    pub text: &'static str,
    pub predicate: Box<Check<V>>,
}

impl<V: Value> Example<V> {
    pub fn new(text: &'static str, predicate: Box<Check<V>>) -> Example<V> {
        Example {
            text: text,
            predicate: predicate,
        }
    }
}

pub trait Check<V: Value>: Debug {
    fn check(&self, value: &ParsedNode<V>) -> bool;
}

pub fn train<V, F, E>(rules: &RuleSet<V>, examples: Vec<Example<V>>, feature_extractor: E)
     -> RustlingResult<Model<RuleId, Truth, F>> 
     where V: Value+Debug+StashIndexable,
           V::Payload: Debug + Eq + Hash,
           F: Feature,
           E: FeatureExtractor<V, F>,
{
    let mut classified_ex: FnvHashMap<RuleId, Vec<(FnvHashMap<F, usize>, Truth)>> =
        FnvHashMap::default();
    for ex in examples.iter() {
        let stash = rules.apply_all(&ex.text.to_lowercase()).unwrap();

        // - keep only full-range parsed nodes
        // - partition them according to the example check value
        let (positive_parsed_nodes, negative_parse_nodes) =
            stash
                .into_iter()
                .filter(|candidate| candidate.root_node.byte_range == ::Range(0, ex.text.len()))
                .partition::<Vec<_>, _>(|candidate| ex.predicate.check(&candidate));
        // - example sanity check
        if positive_parsed_nodes.is_empty() {
            Err(format_err!("example: {:?} matched no rule", ex.text))?
        }

        // - expand parse nodes to nodes, according to the partition
        let mut negative_nodes = FnvHashSet::default();
        let mut positive_nodes = FnvHashSet::default();

        fn add_to_set<Payload: Clone + Eq + Hash>(nodes: &mut FnvHashSet<Node<Payload>>, node: &Node<Payload>) {
            nodes.insert(node.clone());
            for child in &node.children {
                add_to_set(nodes, child);
            }
        }

        for parsed_node in positive_parsed_nodes {
            add_to_set(&mut positive_nodes, &parsed_node.root_node);
        }
        for parsed_node in negative_parse_nodes {
            add_to_set(&mut negative_nodes, &parsed_node.root_node);
        }

        // - ignore negative nodes if there is a matching positive node
        for pos in &positive_nodes {
            negative_nodes.remove(pos);
        }
        // - put node counted features, with truth value in the trainable hashmaps
        for (nodes, truth) in vec![(positive_nodes, true), (negative_nodes, false)].into_iter() {
            for n in nodes.into_iter() {
                let mut counted_features = FnvHashMap::default();
                for f in feature_extractor.for_node(&n).features {
                    *counted_features.entry(f).or_insert(0) += 1;
                }
                classified_ex
                    .entry(::RuleId(n.rule_sym))
                    .or_insert(vec![])
                    .push((counted_features, ::Truth(truth)));
            }
        }
    }
    // - train the classifiers
    let classifiers = classified_ex
        .into_iter()
        .map(|(id, examples)| (id, Classifier::train(&examples)))
        .collect();
    Ok(::Model { classifiers: classifiers })
}
