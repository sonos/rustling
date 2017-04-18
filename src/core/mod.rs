#[macro_use]
mod macros;
mod pattern;
mod rule;

use core::rule::Rule;
use core::pattern::Range;

pub trait AttemptFrom<V>: Sized {
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
pub struct ParsedNode<V: Clone> {
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

pub type Stash<V> = Vec<ParsedNode<V>>;

struct RuleSet<'a, StashValue: Clone>(Vec<Box<Rule<'a, StashValue>>>);

impl<'a, V: Clone> RuleSet<'a, V> {
    fn apply_once(&self, stash: &mut Stash<V>, sentence: &'a str) {
        let produced_nodes: Vec<ParsedNode<V>> = self.0
            .iter()
            .flat_map(|rule| rule.apply(stash, sentence).into_iter())
            .collect();
        stash.extend(produced_nodes)
    }

    fn apply_all(&self, sentence: &'a str) -> Stash<V> {
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
    use std::str::FromStr;
    use core::*;

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
            |a,_| a.value*1000
        };
        let rule_set = RuleSet(vec![rule, rule_compo, rule_thousand]);
        let output_stash = rule_set.apply_all("foobar: 12 thousands");
        assert_eq!(3, output_stash.len());
        let values: Vec<_> = output_stash.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![12, 1000, 12000], values);
    }

    #[test]
    fn test_integer_numeric_infix_rule() {
        let rule_int =
            rule! { "int", (reg!(usize, "\\d+")), |a| usize::from_str(&*a.0[0]).unwrap() };
        let rule_add = rule! {
            "add",
            (dim!(usize), reg!(usize, "\\+"), dim!(usize)),
            |a,_,b| a.value+b.value
        };
        let rule_mul = rule! {
            "mul",
            (dim!(usize), reg!(usize, "\\*"), dim!(usize)),
            |a,_,b| a.value*b.value
        };
        let rule_set = RuleSet(vec![rule_int, rule_add, rule_mul]);
        let results = rule_set.apply_all("foo: 12 + 42, 12* 42");
        let values: Vec<_> = results.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![12, 42, 12, 42, 54, 504], values);
    }

    #[derive(Copy,Clone,PartialEq,Debug)]
    enum Value {
        UI(usize),
        FP(f32),
    }
    impl From<f32> for Value {
        fn from(f: f32) -> Value {
            Value::FP(f)
        }
    }
    impl From<usize> for Value {
        fn from(v: usize) -> Value {
            Value::UI(v)
        }
    }
    impl AttemptFrom<Value> for f32 {
        fn attempt_from(v: Value) -> Option<f32> {
            if let Value::FP(f) = v { Some(f) } else { None }
        }
    }
    impl AttemptFrom<Value> for usize {
        fn attempt_from(v: Value) -> Option<usize> {
            if let Value::UI(f) = v { Some(f) } else { None }
        }
    }

    #[test]
    fn test_with_enum_value() {
        let int = rule! { "int", (reg!(Value, "\\d+")),
                |a| usize::from_str(&*a.0[0]).unwrap() };
        let fp = rule! { "fp", (reg!(Value, "\\d+.\\d+")),
                |a| f32::from_str(&*a.0[0]).unwrap() };
        let pow = rule! { "pow",
            (dim!(f32), reg!(Value, "\\^"), dim!(usize)),
           |a,_,b| a.value.powi(b.value as i32) };
        let rule_set = RuleSet(vec![int, fp, pow]);
        let results = rule_set.apply_all("foo: 1.5^2");
        let values: Vec<_> = results.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![Value::UI(1), Value::UI(5), Value::UI(2), Value::FP(1.5), Value::FP(2.25)],
                   values);
    }
}
