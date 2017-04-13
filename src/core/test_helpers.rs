use regex::Regex;

use core::*;
use core::rule::*;
use core::pattern::*;

#[derive(Debug,PartialEq,Copy,Clone)]
pub enum Value {
    Int { value: i64, grain: u8, group: bool },
    Unknown,
}

// #[derive(Debug)]
// pub struct IntegerNodePredicate {
// pub min: i64,
// pub max: i64,
// }
//
// impl NodePredicate<Value> for IntegerNodePredicate {
// fn predicate(&self, node: &ParsedNode<Value>) -> bool {
// if let Value::Int { value, .. } = node.value {
// self.min <= value && value <= self.max
// } else {
// false
// }
// }
// }
//
// pub fn integer_numeric_en_pattern() -> RegexPattern<Value> {
// RegexPattern::new(Regex::new(r#"(\d{1,2})"#).unwrap())
// }
//
// pub fn integer_numeric_en_pattern_more() -> RegexPattern<Value> {
// RegexPattern::new(Regex::new(r#"(\d{3,4})"#).unwrap())
// }
//
// pub fn integer_numeric_en_rule() -> Rule<Value> {
// let rule_name = "integer (numeric)";
// Rule {
// name: rule_name,
// patterns: vec![Box::new(integer_numeric_en_pattern())],
// production: Box::new(ProducerIntegerNumericTest(rule_name)),
// }
// }
//
// pub fn integer_numeric_en_rule_more() -> Rule<Value> {
// let rule_name = "integer (numeric)";
// Rule {
// name: rule_name,
// patterns: vec![Box::new(integer_numeric_en_pattern_more())],
// production: Box::new(ProducerIntegerNumericTest(rule_name)),
// }
// }
//
//
// pub fn integer_numeric_twice_en_rule() -> Rule<Value> {
// let rule_name = "integer (numeric)";
// Rule {
// name: rule_name,
// patterns: vec![Box::new(integer_numeric_en_pattern()),
// Box::new(integer_numeric_en_pattern())],
// production: Box::new(ProducerIntegerNumericTest(rule_name)),
// }
// }
//
// pub fn integer_composition_en_rule() -> Rule<Value> {
// let rule_name = "Composte Integer (numeric)";
// Rule {
// name: rule_name,
// patterns: vec![Box::new(NodePattern {
// predicate: Box::new(IntegerNodePredicate {
// min: 1,
// max: 10000,
// }),
// }),
// Box::new(RegexPattern::new(Regex::new(r#"and"#).unwrap())),
// Box::new(NodePattern {
// predicate: Box::new(IntegerNodePredicate {
// min: 1,
// max: 10000,
// }),
// })],
// production: Box::new(ProducerCompositionIntegerTest(rule_name)),
// }
// }
//
// pub fn integer_node_value(value: i64, range: Range) -> ParsedNode<Value> {
// ParsedNode {
// root_node: Node {
// rule_name: "integer (numeric) test",
// range: range,
// children: vec![],
// },
// value: Value::Int {
// value: value,
// grain: 1,
// group: false,
// },
// latent: false,
// }
// }
//
