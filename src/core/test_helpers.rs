use regex::Regex;

use core::*;
use core::rule::*;
use core::pattern::*;

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum Dimension {
    Int,
}

#[derive(Debug,PartialEq,Copy,Clone)]
pub enum Value {
    Int { value: i64, grain: u8, group: bool },
    Unknown,
}

impl ::core::Value for Value {}

#[derive(Debug)]
struct ProducerIntegerNumericTest(&'static str);

impl Producer<Value> for ProducerIntegerNumericTest {
    fn produce(&self, matches: &Vec<Match<Value>>, sentence: &str) -> ParsedNode<Value> {
        let start = matches[0].start();
        let end = matches[matches.len() - 1].end();
        let full_range = Range::new(start, end);

        let first_text_match = if let Match::Text(ref text_ranges, _) = matches[0] {
            text_ranges
        } else {
            panic!("Error")
        };
        let integer_value_string = &sentence[first_text_match[1].start..first_text_match[1].end];
        let integer: i64 = integer_value_string.parse().unwrap();


        ParsedNode {
            root_node: Node {
                rule_name: self.0,
                range: full_range,
                children: matches.iter().map(|match_| match_.to_node()).collect(),
            },
            value: Value::Int {
                value: integer,
                grain: 1,
                group: false,
            },
            latent: false,
        }
    }
}

#[derive(Debug)]
struct ProducerCompositionIntegerTest(&'static str);

impl Producer<Value> for ProducerCompositionIntegerTest {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn produce(&self, matches: &Vec<Match<Value>>, _sentence: &str) -> ParsedNode<Value> {
        let start = matches[0].start();
        let end = matches[matches.len() - 1].end();
        let full_range = Range::new(start, end);

        let first_node = if let Match::ParsedNode(ref first_node) = matches[0] { first_node } else { panic!("Error") };
        let second_node = if let Match::ParsedNode(ref second_node) = matches[2] { second_node } else { panic!("Error") };
        let first_integer_value = if let Value::Int { value, .. } = first_node.value { value } else { panic!("Error") };
        let second_integer_value = if let Value::Int  { value, .. } = second_node.value { value } else { panic!("Error") }; 

        ParsedNode {
            root_node: Node {
                rule_name: self.0,
                range: full_range,
                children: matches.iter().map(|match_| match_.to_node()).collect(),
            },
            value: Value::Int {
                value: first_integer_value + second_integer_value,
                grain: 1,
                group: false,
            },
            latent: false,
        }
    }
}

#[derive(Debug)]
pub struct IntegerNodePredicate {
    pub min: i64,
    pub max: i64,
}

impl NodePredicate<Value> for IntegerNodePredicate {
    fn predicate(&self, node: &ParsedNode<Value>) -> bool {
        if let Value::Int { value, .. } = node.value {
            self.min <= value && value <= self.max
        } else {
            false
        }
    }
}

pub fn integer_numeric_en_pattern() -> RegexPattern<Value> {
    RegexPattern::new(Regex::new(r#"(\d{1,2})"#).unwrap())
}

pub fn integer_numeric_en_pattern_more() -> RegexPattern<Value> {
    RegexPattern::new(Regex::new(r#"(\d{3,4})"#).unwrap())
}

pub fn integer_numeric_en_rule() -> Rule<Value> {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}

pub fn integer_numeric_en_rule_more() -> Rule<Value> {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern_more())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}


pub fn integer_numeric_twice_en_rule() -> Rule<Value> {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern()),
                       Box::new(integer_numeric_en_pattern())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}

pub fn integer_composition_en_rule() -> Rule<Value> {
    let rule_name = "Composte Integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(NodePattern {
                           predicate: Box::new(IntegerNodePredicate {
                               min: 1,
                               max: 10000,
                           }),
                       }),
                       Box::new(RegexPattern::new(Regex::new(r#"and"#).unwrap())),
                       Box::new(NodePattern {
                           predicate: Box::new(IntegerNodePredicate {
                               min: 1,
                               max: 10000,
                           }),
                       })],
        production: Box::new(ProducerCompositionIntegerTest(rule_name)),
    }
}

pub fn integer_node_value(value: i64, range: Range) -> ParsedNode<Value> {
    ParsedNode {
        root_node: Node {
            rule_name: "integer (numeric) test",
            range: range,
            children: vec![],
        },
        value: Value::Int {
            value: value,
            grain: 1,
            group: false,
        },
        latent: false,
    }
}
