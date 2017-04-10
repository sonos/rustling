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

#[derive(Debug)]
struct Rule {
    pub name: String,
    pub patterns: Vec<Box<Pattern>>,
}

impl Rule {
    fn apply(&self, stash: &Stash, sentence: &str) -> Vec<Node> {
        unimplemented!();
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

#[derive(Debug, PartialEq, Clone, Copy)]
struct Node {
    pub range: Range,
    pub dimension: Dimension,
    pub latent: bool, //    pub content: String,
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

    fn integer_numeric_en_pattern() -> RegexPattern {
        RegexPattern(Regex::new(r#"(\d{1,18})"#).unwrap())
    }

    fn integer_numeric_en_rule() -> Rule {
        Rule {
            name: "integer (numeric)".into(),
            patterns: vec![Box::new(integer_numeric_en_pattern())],
        }
    }

    fn integer_numeric_twice_en_rule() -> Rule {
        Rule {
            name: "integer (numeric)".into(),
            patterns: vec![Box::new(integer_numeric_en_pattern()),
                           Box::new(integer_numeric_en_pattern())],
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
    fn integer_stuff() {
        let rules = RuleSet(vec![integer_numeric_en_rule()]);
    }
}
