use std::fmt::Debug;
use std::collections::HashMap;
use std::collections::HashSet;

use {Classifier, Feature, FeatureExtractor, Model, Node, ParsedNode, RuleId, RuleSet, Truth, Value};
use DucklingResult;

#[derive(Debug)]
pub struct Example<V: Value> {
    text: &'static str,
    predicate: Box<Check<V>>,
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
    fn check(&self, &ParsedNode<V>) -> bool;
}

pub fn train<V: Value, F: Feature, E: FeatureExtractor<V, F>>
    (rules: &RuleSet<V>,
     examples: Vec<Example<V>>,
     feature_extractor: E)
     -> DucklingResult<Model<RuleId, Truth, F>> {
    let mut classified_ex: HashMap<RuleId, Vec<(HashMap<F, usize>, Truth)>> = HashMap::new();
    for ex in examples.iter() {
        let stash = rules.apply_all(&ex.text.to_lowercase()).unwrap();

        // - keep only full-range parsed nodes
        // - partition them according to the example check value
        let (positive_parsed_nodes, negative_parse_nodes) = stash.into_iter()
            .filter(|candidate| candidate.root_node.range == ::Range(0, ex.text.len()))
            .partition::<Vec<_>, _>(|candidate| ex.predicate.check(&candidate));

        // - example sanity check
        if positive_parsed_nodes.is_empty() {
            Err(format!("example: {:?} matched no rule", ex))?
        }

        // - expand parse nodes to nodes, according to the partition
        let mut negative_nodes = HashSet::new();
        let mut positive_nodes = HashSet::new();
        fn add_to_set(nodes: &mut HashSet<Node>, node: &Node) {
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
                let mut counted_features = HashMap::new();
                for f in feature_extractor.for_node(&n).features {
                    *counted_features.entry(f).or_insert(0) += 1;
                }
                classified_ex.entry(::RuleId(n.rule_name))
                    .or_insert(vec![])
                    .push((counted_features, ::Truth(truth)));
            }
        }
    }
    // - train the classifiers
    let classifiers = classified_ex.into_iter()
        .map(|(id, examples)| (id, Classifier::train(&examples)))
        .collect();
    Ok(::Model { classifiers: classifiers })
}
