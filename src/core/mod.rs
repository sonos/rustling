mod rule;
mod pattern;

#[cfg(test)]
mod test_helpers;

use ontology::Value;
use core::rule::{Range, Rule};

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedNode {
    pub root_node: Node,
    pub value: Value,
    pub latent: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub rule_name: &'static str,
    pub range: Range,
    pub children: Vec<Node>,
}

type Stash = Vec<ParsedNode>;

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
        for _ in 0..iterations_max {
            self.apply_once(&mut stash, sentence);
            if stash.len() <= previous_stash_size && stash.len() > max_stash_size { break; }
            previous_stash_size = stash.len();
        }
        stash
    }
}

#[cfg(test)]
mod tests {
    use core::*;
    use core::test_helpers::*;

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
