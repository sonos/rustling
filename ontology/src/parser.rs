use duckling::*;
use ::*;
use en;
use core::errors::*;
use std::collections::HashMap;
use core::pattern::Range;
use std::cmp::Ordering;
use duckling::errors::DucklingResult;

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Feat(Vec<&'static str>);
impl ml::Feature for Feat {}

pub fn build_parser_en() -> DucklingResult<duckling::Parser<Dimension, Feat, FeatureExtractor>> {
    let rules = en::rules_numbers()?;
    let exs = ::examples::examples_en_numbers();
    let model = duckling::train::train(&rules, exs, FeatureExtractor())?;
    Ok(duckling::Parser::new(rules, model, FeatureExtractor()))
}

pub struct FeatureExtractor();

impl duckling::FeatureExtractor<Dimension, Feat> for FeatureExtractor {
    fn for_parsed_node(&self, node:&ParsedNode<Dimension>) -> ml::Input<duckling::RuleId, Feat> {
        self.for_node(&node.root_node)
    }
    fn for_node(&self, node:&Node) -> ml::Input<duckling::RuleId, Feat> {
        extract_node_features(&node)
    }
}

pub fn extract_node_features(node:&core::Node) -> ml::Input<duckling::RuleId, Feat> {
    let features = vec![
        Feat(node.children.iter().map({ |child| child.rule_name }).collect())
    ];

    let children_features = node.children
        .iter()
        .map({ |child| extract_node_features(child) })
        .collect();

    ml::Input {
        classifier_id: duckling::RuleId(node.rule_name),
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
