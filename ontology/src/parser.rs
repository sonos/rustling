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
struct Id(&'static str);
impl ml::ClassifierId for Id {}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
struct Class(bool);
impl ml::ClassId for Class {}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
struct Feat(Vec<&'static str>);
impl ml::Feature for Feat {}

struct Parser<'a> {
    rules: RuleSet<'a, Dimension>,
    model: Model<Id, Class, Feat>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserMatch {
    range: Range,
    value: Dimension,
    probalog: f32,
}

impl PartialOrd for ParserMatch {
    fn partial_cmp(&self, other: &ParserMatch) -> Option<Ordering> {
        unimplemented!()
    }
}

impl<'a> Parser<'a> {
    pub fn new() -> Result<Parser<'a>> {
        return Ok(Parser {
            rules: en::rules_numbers()?,
            model: Model { classifiers: HashMap::new() }
        })
    }

    pub fn parse(&self, input: &'a str) -> Result<Vec<ParserMatch>> {

        let candidates = self.rules
            .apply_all(input)?
            .into_iter().map(|p| {
                let features: Input<Id, Feat> = p.extract_features();
                let probalog = self.model.classify(&features, &Class(true)).unwrap(); //FIXME: return a result
                ParserMatch {
                    range: p.root_node.range,
                    value: p.value,
                    probalog: probalog
                }
            })
            .collect();

        let winners = self.select_winners(candidates);
        Ok(winners)
    }

    fn select_winners(&self, candidates: Vec<ParserMatch>) -> Vec<ParserMatch> {
        candidates
    }
}

trait FeatureExtractor {
    fn extract_features(&self) -> Input<Id, Feat>;
}

impl<V: Clone> FeatureExtractor for ParsedNode<V> {
    fn extract_features(&self) -> Input<Id, Feat> {
        self.root_node.extract_features()
    }
}

impl FeatureExtractor for Node {
    fn extract_features(&self) -> Input<Id, Feat> {
        let features = vec![
            Feat(self.children.iter().map({ |child| child.rule_name }).collect())
        ];

        let children_features = self.children
            .iter()
            .map({ |child| child.extract_features() })
            .collect();

        Input {
            classifier_id: Id(self.rule_name),
            features: features,
            children: children_features,
        }
    }
}
 
#[cfg(test)]
mod tests {
    use parser::*;

    #[test]
    fn test_parser() {
        let parser = Parser::new().unwrap();
        let result = parser.parse("twenty").unwrap();
        assert_eq!(vec![ParserMatch {
            range: (0, 6),
            value: IntegerValue::new_with_grain(20, 1).unwrap().into(),
            probalog: 0.0,
        }], result);
    }
}