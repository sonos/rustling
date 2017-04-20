use core::RuleSet;
use core::{ Stash, ParsedNode, Node };
use ml::*;
use ::*;
use en;
use core::errors::*;
use std::collections::HashMap;
use core::pattern::Range;
use std::cmp::Ordering;

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
struct Feat(Vec<&'static str>);
impl ml::Feature for Feat {}

fn build_parser_en<'a>() -> Result<duckling::Parser<'a,Dimension, Feat, FeatureExtractor>> {
    Ok(duckling::Parser::new(en::rules_numbers()?, ml::Model { classifiers: HashMap::new() }))
}

struct FeatureExtractor();

impl duckling::FeatureExtractor<Dimension, Feat> for FeatureExtractor {
    fn extract_features(node:&ParsedNode<Dimension>) -> Input<duckling::Id, Feat> {
        extract_node_features(&node.root_node)
    }
}

fn extract_node_features(node:&core::Node) -> Input<duckling::Id, Feat> {
    let features = vec![
        Feat(node.children.iter().map({ |child| child.rule_name }).collect())
    ];

    let children_features = node.children
        .iter()
        .map({ |child| extract_node_features(child) })
        .collect();

    Input {
        classifier_id: duckling::Id(node.rule_name),
        features: features,
        children: children_features,
    }
}

#[cfg(test)]
mod tests {
    use parser::*;
    use duckling::ParserMatch;
    use core::pattern::Range;

    #[test]
    fn test_twenty() {
        let parser = build_parser_en().unwrap();
        let result = parser.parse("twenty", |_| Some(1)).unwrap();
        assert_eq!(vec![ParserMatch {
            range: Range(0, 6),
            value: IntegerValue::new_with_grain(20, 1).unwrap().into(),
            probalog: 0.0,
        }], result);
    }

    #[test]
    fn test_21() {
        let parser = build_parser_en().unwrap();
        let result = parser.parse("twenty-one", |_| Some(1)).unwrap();
        panic!("{:?}", result);
        panic!();
    }

    #[test]
    fn test_2_1000() {
        let parser = build_parser_en().unwrap();
        let result = parser.parse("twenty-one thousands", |_| Some(1)).unwrap();
        panic!("{:?}", result);
        panic!();
    }

    #[test]
    fn test_foobar() {
        let parser = build_parser_en().unwrap();
        let result = parser.parse("foobar twenty thousands", |_| Some(1)).unwrap();
        panic!("{:?}", result);
    }
}
