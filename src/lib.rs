extern crate regex;

use std::fmt::Debug;

#[derive(Debug, PartialEq, Clone, Copy)]
struct Range {
    pub start: usize,
    pub end: usize, // excluded
}

impl Range {
    fn new(start: usize, end: usize) -> Range {
        Range {
            start: start,
            end: end,
        }
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
enum Dimension {
    Int,
}

#[derive(Debug,PartialEq,Copy,Clone)]
enum Value {
    Int { value: i64, grain: u8, group: bool }, /*    Number(NumberKind), Float(f64), Temperature, Time, DurationUnit, Duration, Price, Cycle, Unit
                                                 * */
    Unknown,
}

impl Value {
    fn dim(&self) -> Dimension {
        match self {
            &Value::Int { .. } => Dimension::Int,
            &Value::Unknown => panic!("Unknown"),
        }
    }
}

trait Producer: Debug {
    fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode;
}

#[derive(Debug)]
struct Rule {
    pub name: &'static str,
    pub patterns: Vec<Box<Pattern>>,
    pub production: Box<Producer>,
}

impl Rule {
    pub fn apply(&self, stash: &Stash, sentence: &str) -> Vec<ParsedNode> {
        // 1 Matches
        //FIXME unify
        let matches = self.matches(stash, sentence);
        matches
            .iter()
            .map(|submatches| self.produce(submatches, sentence))
            .collect()
    }

    fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode {
        self.production.produce(matches, sentence)
    }

    fn matches(&self, stash: &Stash, sentence: &str) -> Vec<Vec<Match>> {
        let new_matches = self.rec_matches(stash, sentence, 0, 0, vec![]);
        new_matches.into_iter()
            .filter(|matches|{
                let nodes: Vec<Node> = matches.into_iter().map(|match_| match_.to_node()).collect();
                stash.iter().all(|parsed_node| {
                    parsed_node.root_node.children != nodes || parsed_node.root_node.rule_name != self.name
                })
            }).collect()
    }

    fn rec_matches(&self,
                   stash: &Stash,
                   sentence: &str,
                   sentence_done: usize,
                   pattern_done: usize,
                   prefix: Vec<Match>)
                   -> Vec<Vec<Match>> {

        if pattern_done >= self.patterns.len() {
            return vec![prefix];
        }
        let mut result = vec![];
        for match_ in self.patterns[pattern_done].predicate(stash, sentence, sentence_done, self.name) {
            assert!(match_.start() >= sentence_done);
            if pattern_done > 0 {
                let spacing = &sentence[sentence_done..match_.start()];
                if !spacing.chars().all(|c| c.is_whitespace() || c == '-') {
                    continue;
                }
            }
            let mut full_prefix = prefix.clone();
            let end = match_.end();
            full_prefix.push(match_);
            let full_matches =
                self.rec_matches(stash, sentence, end, pattern_done + 1, full_prefix);
            result.extend(full_matches);
        }
        result
    }
}

type Stash = Vec<ParsedNode>;
type Tokens = Vec<Node>;

trait Pattern: Debug {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match>;
}

#[derive(PartialEq, Debug, Clone)]
enum Match {
    ParsedNode(ParsedNode),
    Text(Vec<Range>, &'static str),
}

impl Match {
    fn start(&self) -> usize {
        match self {
            &Match::Text(ref v, _) => v[0].start,
            &Match::ParsedNode(ref n) => n.root_node.range.start,
        }
    }

    fn end(&self) -> usize {
        match self {
            &Match::Text(ref v, _) => v[0].end,
            &Match::ParsedNode(ref n) => n.root_node.range.end,
        }
    }

    fn range(&self) -> Range {
        Range::new(self.start(), self.end())
    }

    fn to_node(&self) -> Node {
        match self {
            &Match::Text(ref v, rule_name) => Node { range: v[0], rule_name: rule_name, children: vec![] },
            &Match::ParsedNode(ref n) => n.root_node.clone(),
        }
    }
}

#[derive(Debug)]
struct RegexPattern(regex::Regex);
impl Pattern for RegexPattern {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match> {
        self.0
            .captures_iter(&sentence[start..])
            .map(|cap| {
                Match::Text(cap.iter()
                                .map(|m| {
                                         // FIXME
                                         let m = m.unwrap();
                                         Range {
                                             start: start + m.start(),
                                             end: start + m.end(),
                                         }
                                     })
                                .collect(),
                                rule_name)
            })
            .collect()
    }
}

trait NodePredicate: Debug {
    fn predicate(&self, node: &ParsedNode) -> bool;
}

#[derive(Debug)]
struct NodePattern {
    predicate: Box<NodePredicate>,
}

impl Pattern for NodePattern {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match> {
        stash
            .iter()
            .filter(|node| self.predicate.predicate(node) && node.root_node.range.start >= start)
            .map(|node| Match::ParsedNode(node.clone()))
            .collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
struct ParsedNode {
    pub root_node: Node,
    pub value: Value,
    pub latent: bool,
}

#[derive(Debug, PartialEq, Clone)]
struct Node {
    pub rule_name: &'static str,
    pub range: Range,
    pub children: Vec<Node>,
}

struct RuleSet(Vec<Rule>);

impl RuleSet {
    fn apply_once(&self, stash: &mut Stash, sentence: &str) {
        let produced_nodes: Vec<ParsedNode> = self.0.iter()
                .flat_map(|rule| rule.apply(stash, sentence).into_iter()).collect();
        stash.extend(produced_nodes)
    }

    fn apply_all(&self, sentence: &str) -> Stash {
        let iterations_max = 10;
        let max_stash_size = 600;
        let mut stash = vec![];
        let mut previous_stash_size = 0;
        for iteration in 0..iterations_max {
            self.apply_once(&mut stash, sentence);
            if stash.len() <= previous_stash_size && stash.len() > max_stash_size { break; }
            previous_stash_size = stash.len();
        }
        stash
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    #[derive(Debug)]
    struct ProducerIntegerNumericTest(&'static str);

    impl Producer for ProducerIntegerNumericTest {
        fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode {
            let start = matches[0].start();
            let end = matches[matches.len() - 1].end();
            let full_range = Range::new(start, end);

            let first_text_match = if let Match::Text(ref text_ranges, _) = matches[0] { text_ranges } else { panic!("Error") };
            let integer_value_string = &sentence[first_text_match[1].start .. first_text_match[1].end];
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
        fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode {
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
    struct IntegerNodePredicate {
        min: i64,
        max: i64,
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

    fn integer_numeric_en_pattern() -> RegexPattern {
        RegexPattern(Regex::new(r#"(\d{1,2})"#).unwrap())
    }

    fn integer_numeric_en_pattern_more() -> RegexPattern {
        RegexPattern(Regex::new(r#"(\d{3,4})"#).unwrap())
    }

    fn integer_numeric_en_rule() -> Rule {
        let rule_name = "integer (numeric)";
        Rule {
            name: rule_name,
            patterns: vec![Box::new(integer_numeric_en_pattern())],
            production: Box::new(ProducerIntegerNumericTest(rule_name)),
        }
    }

    fn integer_numeric_en_rule_more() -> Rule {
        let rule_name = "integer (numeric)";
        Rule {
            name: rule_name,
            patterns: vec![Box::new(integer_numeric_en_pattern_more())],
            production: Box::new(ProducerIntegerNumericTest(rule_name)),
        }
    }


    fn integer_numeric_twice_en_rule() -> Rule {
        let rule_name = "integer (numeric)";
        Rule {
            name: rule_name,
            patterns: vec![Box::new(integer_numeric_en_pattern()),
                           Box::new(integer_numeric_en_pattern())],
            production: Box::new(ProducerIntegerNumericTest(rule_name)),
        }
    }

    fn integer_composition_en_rule() -> Rule {
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

    fn integer_node_value(value: i64, range: Range) -> ParsedNode {
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

    macro_rules! r {
        ($s:expr,$e:expr) => ( Range::new($s, $e) )
    }

    #[test]
    fn test_integer_node_predicate() {
        let pattern =
            NodePattern { predicate: Box::new(IntegerNodePredicate { min: 1, max: 100 }) };
        let sentence = "foobar: 12";
        let stash = vec![integer_node_value(12, r!(8, 10))];

        assert_eq!(1, pattern.predicate(&stash, sentence, 0, "rule_name").len())
    }

    #[test]
    fn test_integer_numeric_en_pattern() {
        let pat = integer_numeric_en_pattern();
        assert_eq!(vec![Match::Text(vec![r!(8, 10), r!(8, 10)], "rule_name")],
                   pat.predicate(&vec![], "foobar: 42", 0, "rule_name"))
    }

    #[test]
    fn test_integer_numeric_en_rule() {
        let rule = integer_numeric_en_rule();
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)], rule.name)]],
                   rule.matches(&vec![], "foobar: 42"));
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)], rule.name)],
                        vec![Match::Text(vec![r!(11, 13), r!(11, 13)], rule.name)]],
                   rule.matches(&vec![], "foobar: 12 42"));
    }

    #[test]
    fn test_integer_numeric_twice_en_rule() {
        let rule = integer_numeric_twice_en_rule();
        assert_eq!(0, rule.matches(&vec![], "foobar: 42").len());
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)], rule.name),
                             Match::Text(vec![r!(11, 13), r!(11, 13)], rule.name)]],
                   rule.matches(&vec![], "foobar: 12 42"));
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)], rule.name),
                             Match::Text(vec![r!(11, 13), r!(11, 13)], rule.name)],
                        vec![Match::Text(vec![r!(11, 13), r!(11, 13)], rule.name),
                             Match::Text(vec![r!(14, 16), r!(14, 16)], rule.name)]],
                   rule.matches(&vec![], "foobar: 12 42 64"));
    }

    #[test]
    fn test_integer_composition_en_rule() {
        let rule = integer_composition_en_rule();
        let sentence = "foobar: 12 and 42";
        let stash = vec![integer_node_value(12, r!(8, 10)),
                         integer_node_value(42, r!(15, 17))];
        assert_eq!(1, rule.matches(&stash, sentence).len());
        assert_eq!(vec![vec![Match::ParsedNode(stash[0].clone()),
                             Match::Text(vec![r!(11, 14)], rule.name),
                             Match::ParsedNode(stash[1].clone())]],
                   rule.matches(&stash, sentence));
    }

    #[test]
    fn test_rule_production_integer_numeric_en() {
        let rule = integer_numeric_en_rule();
        let parsed_nodes = rule.apply(&vec![], "foobar: 42");
        assert_eq!(1, parsed_nodes.len());
        assert_eq!(r!(8, 10), parsed_nodes[0].root_node.range);
        assert_eq!(rule.name, parsed_nodes[0].root_node.rule_name);
        assert_eq!(Value::Int { value: 42, grain: 1, group: false}, parsed_nodes[0].value);
    }

    #[test]
    fn test_rule_production_integer_numeric_twice_en() {
        let rule = integer_numeric_twice_en_rule();
        let parsed_nodes = rule.apply(&vec![], "foobar: 12 42 64");
        assert_eq!(2, parsed_nodes.len());
        assert_eq!(r!(8, 13), parsed_nodes[0].root_node.range);
        assert_eq!(rule.name, parsed_nodes[0].root_node.rule_name);
        assert_eq!(r!(11, 16), parsed_nodes[1].root_node.range);
        assert_eq!(rule.name, parsed_nodes[1].root_node.rule_name);
    }

    #[test]
    fn test_rule_production_integer_numeric_composition_en() {
        let rule = integer_composition_en_rule();
        let sentence = "foobar: 12 and 42";
        let stash = vec![integer_node_value(12, r!(8, 10)),
                         integer_node_value(42, r!(15, 17))];
        let parsed_nodes = rule.apply(&stash, sentence);
        assert_eq!(1, parsed_nodes.len());
        assert_eq!(Value::Int { value: 54, grain: 1, group: false}, parsed_nodes[0].value);
    }

    #[test]
    fn test_rule_set_application_once() {
        let rule = integer_numeric_en_rule();
        let rule_set = RuleSet(vec![rule]);
        let mut stash = vec![];
        rule_set.apply_once(&mut stash, "foobar: 42");
        assert_eq!(1, stash.len());
        assert_eq!(Value::Int { value: 42, grain: 1, group: false}, stash[0].value);
    }

    #[test]
    fn test_rule_set_application_all() {
        let rule = integer_numeric_en_rule();
        let rule_set = RuleSet(vec![
                integer_numeric_en_rule(),
                integer_numeric_en_rule_more(),
                integer_composition_en_rule(), 
            ]);
        let output_stash = rule_set.apply_all("foobar: 12 and 142");
        println!("{:?}", output_stash.iter().map(|parsed_node| parsed_node.root_node.range).collect::<Vec<_>>());
        assert_eq!(6, output_stash.len());
    }
}
