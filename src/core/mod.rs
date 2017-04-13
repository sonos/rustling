
use core::pattern::TextPattern;

macro_rules! reg {
    ($typ:ty, $pattern:expr) => ( $crate::core::pattern::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
}

macro_rules! rule {
    ($name:expr, ($a:expr), $f:expr) => { Box::new($crate::core::rule::Rule1::new($name, $a, $f)) };
    ($name:expr, ($a:expr, $b:expr), $f:expr) => { Box::new($crate::core::rule::Rule2::new($name, ($a, $b), $f)) };
//    (($a:expr, $b:expr, $c:expr), $f:expr) => { Box::new(Rule3::new(($a, $b, $c), $f)) };
}

macro_rules! dim {
	($typ:ty) => ( $crate::core::pattern::AnyNodePattern::<$typ>::new() );
	($typ:ty, $predicates:expr) => ( $crate::core::pattern::DimensionNodePattern::<$typ,_>::filter($predicates) );
}

mod pattern;
#[cfg(test)]
mod test_helpers;
mod rule;

use core::rule::Rule;
use core::pattern::Range;

trait AttemptFrom<V>: Sized {
    fn attempt_from(v: V) -> Option<Self>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub rule_name: &'static str,
    pub range: Range,
    pub children: Vec<Node>,
}

impl Node {
    fn new(name: &'static str, range: Range, children: Vec<Node>) -> Node {
        Node {
            rule_name: name,
            range: range,
            children: children,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct ParsedNode<V: Clone> {
    pub root_node: Node,
    pub value: V,
    pub latent: bool,
}

impl<V: Clone> ParsedNode<V> {
    fn new(name: &'static str, v: V, r: Range, children: Vec<Node>) -> ParsedNode<V> {
        ParsedNode {
            root_node: Node::new(name, r, children),
            value: v,
            latent: false,
        }
    }
}

type Stash<V> = Vec<ParsedNode<V>>;

struct RuleSet<StashValue: Clone>(Vec<Box<Rule<StashValue>>>);

impl<V: Clone> RuleSet<V> {
    fn apply_once(&self, stash: &mut Stash<V>, sentence: &str) {
        let produced_nodes: Vec<ParsedNode<V>> = self.0
            .iter()
            .flat_map(|rule| rule.apply(stash, sentence).into_iter())
            .collect();
        stash.extend(produced_nodes)
    }

    fn apply_all(&self, sentence: &str) -> Stash<V> {
        let iterations_max = 10;
        let max_stash_size = 600;
        let mut stash = vec![];
        let mut previous_stash_size = 0;
        for _ in 0..iterations_max {
            self.apply_once(&mut stash, sentence);
            if stash.len() <= previous_stash_size && stash.len() > max_stash_size {
                break;
            }
            previous_stash_size = stash.len();
        }
        stash
    }
}

#[cfg(test)]
mod tests {
    use core::*;
    //    use core::test_helpers::*;

    impl AttemptFrom<usize> for usize {
        fn attempt_from(v: usize) -> Option<usize> {
            Some(v)
        }
    }

    #[test]
    fn test_rule_set_application_once() {
        let rule = rule! {
            "integer (numeric)",
            (reg!(usize, r#"(\d{1,18})"#)),
            |text_match| text_match.0[0].parse::<usize>().unwrap()
        };
        let rule_set = RuleSet(vec![rule]);
        let mut stash = vec![];
        rule_set.apply_once(&mut stash, "foobar: 42");
        assert_eq!(1, stash.len());
        assert_eq!(42, stash[0].value);
    }

    #[test]
    fn test_rule_set_application_all() {
        let rule = rule! {
            "integer (numeric)",
            (reg!(usize, r#"(\d{1,18})"#)),
            |text_match| text_match.0[0].parse::<usize>().unwrap()
        };
        let rule_thousand = rule! {
            "integer (thousand)",
            (reg!(usize, "thousands?")),
            |_| 1000usize
        };
        let rule_compo = rule! {
            "number thousands",
            (   dim!(usize, vec!(|a:&usize| *a > 1 && *a < 99)),
                dim!(usize, vec!(|a:&usize| *a == 1000))),
            |a,b| a.value*1000
        };
        let rule_set = RuleSet(vec![rule, rule_compo, rule_thousand]);
        let output_stash = rule_set.apply_all("foobar: 12 thousands");
        println!("{:?}", output_stash);
        assert_eq!(3, output_stash.len());
        let values:Vec<_> = output_stash.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![12, 1000, 12000], values);
    }

}
