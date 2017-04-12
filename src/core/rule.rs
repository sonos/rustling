use std::fmt::Debug;

use core::{Node, ParsedNode, Stash};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub start: usize,
    pub end: usize, // excluded
}

impl Range {
    pub fn new(start: usize, end: usize) -> Range {
        Range {
            start: start,
            end: end,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Match {
    ParsedNode(ParsedNode),
    Text(Vec<Range>, &'static str),
}

impl Match {
    pub fn start(&self) -> usize {
        match self {
            &Match::Text(ref v, _) => v[0].start,
            &Match::ParsedNode(ref n) => n.root_node.range.start,
        }
    }

    pub fn end(&self) -> usize {
        match self {
            &Match::Text(ref v, _) => v[0].end,
            &Match::ParsedNode(ref n) => n.root_node.range.end,
        }
    }

    pub fn range(&self) -> Range {
        Range::new(self.start(), self.end())
    }

    pub fn to_node(&self) -> Node {
        match self {
            &Match::Text(ref v, rule_name) => Node { range: v[0], rule_name: rule_name, children: vec![] },
            &Match::ParsedNode(ref n) => n.root_node.clone(),
        }
    }
}

pub trait Pattern: Debug {
    fn predicate(&self, stash: &Stash, sentence: &str, start: usize, rule_name: &'static str) -> Vec<Match>;
}

pub trait Producer: Debug {
    fn produce(&self, matches: &Vec<Match>, sentence: &str) -> ParsedNode;
}

#[derive(Debug)]
pub struct Rule {
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


#[cfg(test)]
mod tests {
    use core::*;
    use core::pattern::*;
    use core::rule::*;
    use core::test_helpers::*;

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
}
