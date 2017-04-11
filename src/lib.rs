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
}

impl Value {
    fn dim(&self) -> Dimension {
        match self {
            &Value::Int { .. } => Dimension::Int,
        }
    }
}

trait Producer: Debug {
    fn produce(&self, matches: &Vec<Match>) -> ParsedNode;
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
        matches.iter().map(|submatches| {
            self.produce(submatches, sentence)
        }).collect()
    }

    fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode {
        self.production.produce(matches)
    }

    fn matches(&self, stash: &Stash, sentence: &str) -> Vec<Vec<Match>> {
        self.rec_matches(stash, sentence, 0, 0, vec![])
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
        for match_ in self.patterns[pattern_done].predicate(stash, sentence, sentence_done) {
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

type Stash = Vec<Node>;
type Tokens = Vec<Node>;

trait Pattern: Debug {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize) -> Vec<Match>;
}

#[derive(PartialEq, Debug, Clone)]
enum Match {
    Node(Node),
    Text(Vec<Range>),
}

impl Match {
    fn start(&self) -> usize {
        match self {
            &Match::Text(ref v) => v[0].start,
            &Match::Node(ref n) => n.range.start,
        }
    }
    fn end(&self) -> usize {
        match self {
            &Match::Text(ref v) => v[0].end,
            &Match::Node(ref n) => n.range.end,
        }
    }

    fn range(&self) -> Range {
        Range::new(self.start(), self.end())
    }
}

#[derive(Debug)]
struct RegexPattern(regex::Regex);
impl Pattern for RegexPattern {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize) -> Vec<Match> {
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
                    .collect())
            })
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
    fn apply_rule(&self, stash: &Stash, input: &str) -> Vec<()> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    #[derive(Debug)]
    struct ProducerTest(&'static str);

    impl Producer for ProducerTest {
        fn produce(&self, matches: &Vec<Match>) -> ParsedNode {
            let start = matches[0].start();
            let end = matches[matches.len() - 1].end();
            let full_range = Range::new(start, end);

            ParsedNode {
                root_node: Node {
                    rule_name: self.0,
                    range: full_range,
                    children: vec!(),
                },
                value: Value::Int {value: 1i64, grain: 2u8, group: false},
                latent: false,
            }
        }
    }

    fn integer_numeric_en_pattern() -> RegexPattern {
        RegexPattern(Regex::new(r#"(\d{1,18})"#).unwrap())
    }

    fn integer_numeric_en_rule() -> Rule {
        let rule_name = "integer (numeric)";
        Rule {
            name: rule_name,
            patterns: vec![Box::new(integer_numeric_en_pattern())],
            production: Box::new(ProducerTest(rule_name)),
        }
    }

    fn integer_numeric_twice_en_rule() -> Rule {
        let rule_name = "integer (numeric)";
        Rule {
            name: rule_name,
            patterns: vec![Box::new(integer_numeric_en_pattern()),
                           Box::new(integer_numeric_en_pattern())],
            production: Box::new(ProducerTest(rule_name)),
        }
    }

    macro_rules! r {
        ($s:expr,$e:expr) => ( Range::new($s, $e) )
    }

    #[test]
    fn test_integer_numeric_en_pattern() {
        let pat = integer_numeric_en_pattern();
        assert_eq!(vec![Match::Text(vec![r!(8, 10), r!(8, 10)])],
                   pat.predicate(&vec![], "foobar: 42", 0))
    }

    #[test]
    fn test_integer_numeric_en_rule() {
        let rule = integer_numeric_en_rule();
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)])]],
                   rule.matches(&vec![], "foobar: 42"));
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)])],
                        vec![Match::Text(vec![r!(11, 13), r!(11, 13)])]],
                   rule.matches(&vec![], "foobar: 12 42"));
    }

    #[test]
    fn test_integer_numeric_twice_en_rule() {
        let rule = integer_numeric_twice_en_rule();
        assert_eq!(0, rule.matches(&vec![], "foobar: 42").len());
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)]),
                             Match::Text(vec![r!(11, 13), r!(11, 13)])]],
                   rule.matches(&vec![], "foobar: 12 42"));
        assert_eq!(vec![vec![Match::Text(vec![r!(8, 10), r!(8, 10)]),
                             Match::Text(vec![r!(11, 13), r!(11, 13)])],
                        vec![Match::Text(vec![r!(11, 13), r!(11, 13)]),
                             Match::Text(vec![r!(14, 16), r!(14, 16)])]],
                   rule.matches(&vec![], "foobar: 12 42 64"));
    }

    #[test]
    fn test_rule_production_integer_numeric_en() {
        let rule = integer_numeric_en_rule();
        let parsed_nodes = rule.apply(&vec!(), "foobar: 42");
        assert_eq!(1, parsed_nodes.len());
        assert_eq!(r!(8, 10), parsed_nodes[0].root_node.range);
        assert_eq!(rule.name, parsed_nodes[0].root_node.rule_name);
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
    fn integer_stuff() {
        let rules = RuleSet(vec![integer_numeric_en_rule()]);
    }
}
