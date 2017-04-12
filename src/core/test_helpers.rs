use regex::Regex;

use core::*;
use core::rule::*;
use core::pattern::*;

#[derive(Debug)]
struct ProducerIntegerNumericTest(&'static str);

impl Producer for ProducerIntegerNumericTest {
    fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode {
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

impl Producer for ProducerCompositionIntegerTest {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn produce(&self, matches: &Vec<Match>, _sentence: &str) -> ParsedNode {
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

impl NodePredicate for IntegerNodePredicate {
    fn predicate(&self, node: &ParsedNode) -> bool {
        if let Value::Int { value, .. } = node.value {
            self.min <= value && value <= self.max
        } else {
            false
        }
    }
}

pub fn integer_numeric_en_pattern() -> RegexPattern {
    RegexPattern(Regex::new(r#"(\d{1,2})"#).unwrap())
}

pub fn integer_numeric_en_pattern_more() -> RegexPattern {
    RegexPattern(Regex::new(r#"(\d{3,4})"#).unwrap())
}

pub fn integer_numeric_en_rule() -> Rule {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}

pub fn integer_numeric_en_rule_more() -> Rule {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern_more())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}


pub fn integer_numeric_twice_en_rule() -> Rule {
    let rule_name = "integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(integer_numeric_en_pattern()),
                       Box::new(integer_numeric_en_pattern())],
        production: Box::new(ProducerIntegerNumericTest(rule_name)),
    }
}

pub fn integer_composition_en_rule() -> Rule {
    let rule_name = "Composte Integer (numeric)";
    Rule {
        name: rule_name,
        patterns: vec![Box::new(NodePattern {
                           predicate: Box::new(IntegerNodePredicate {
                               min: 1,
                               max: 10000,
                           }),
                       }),
                       Box::new(RegexPattern(Regex::new(r#"and"#).unwrap())),
                       Box::new(NodePattern {
                           predicate: Box::new(IntegerNodePredicate {
                               min: 1,
                               max: 10000,
                           }),
                       })],
        production: Box::new(ProducerCompositionIntegerTest(rule_name)),
    }
}

pub fn integer_node_value(value: i64, range: Range) -> ParsedNode {
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
