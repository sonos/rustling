#[macro_use]
extern crate error_chain;
pub extern crate regex;
extern crate smallvec;

#[macro_use]
pub mod macros;
pub mod pattern;
pub mod rule;

use rule::Rule;
use pattern::Range;
use errors::*;

#[macro_export]
macro_rules! duckling_value {
    ( $name:ident $($varname:ident($varty:ty)),*, ) => {
        #[derive(Debug,Clone, PartialEq)]
        pub enum $name {
            $( $varname($varty) ),*
        }

        $(
            impl From<$varty> for $name {
                fn from(v: $varty) -> $name {
                    $name::$varname(v)
                }
            }
            impl AttemptFrom<$name> for $varty {
                fn attempt_from(v: $name) -> Option<$varty> {
                    if let $name::$varname(value) = v {
                        Some(value)
                    } else {
                        None
                    }
                }
            }
        )*
    }
}

pub mod errors {
    error_chain! {
        foreign_links {
            Regex(::regex::Error);
        }

        errors {
            ProductionRuleError(t: String)
        }
    }
}

pub trait AttemptFrom<V>: Sized {
    fn attempt_from(v: V) -> Option<Self>;
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

pub struct RuleSet<StashValue: Clone>(pub Vec<Box<Rule<StashValue>>>);

impl<V: Clone> RuleSet<V> {
    fn apply_once(&self, stash: &mut Stash<V>, sentence: &str) -> Result<()> {
        let mut produced_nodes = vec!();
        for rule in &self.0 {
            produced_nodes.extend(rule.apply(stash, sentence)?);
        }
        stash.extend(produced_nodes);
        Ok(())
    }

    pub fn apply_all(&self, sentence: &str) -> Result<Stash<V>> {
        let iterations_max = 10;
        let max_stash_size = 600;
        let mut stash = vec![];
        let mut previous_stash_size = 0;
        for _ in 0..iterations_max {
            self.apply_once(&mut stash, sentence)?;
            if stash.len() <= previous_stash_size && stash.len() > max_stash_size {
                break;
            }
            previous_stash_size = stash.len();
        }
        Ok(stash)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use ::*;

    impl AttemptFrom<usize> for usize {
        fn attempt_from(v: usize) -> Option<usize> {
            Some(v)
        }
    }

    macro_rules! reg {
        ($typ:ty, $pattern:expr) => ( $crate::pattern::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
    }

    #[test]
    fn test_rule_set_application_once() {
        let rule = rule! {
            "integer (numeric)",
            (reg!(usize, r#"(\d{1,18})"#)),
            |text_match| Ok(text_match.group(0).parse::<usize>()?)
        };
        let rule_set = RuleSet(vec![rule]);
        let mut stash = vec![];
        rule_set.apply_once(&mut stash, "foobar: 42").unwrap();
        assert_eq!(1, stash.len());
        assert_eq!(42, stash[0].value);
    }

    fn rules() -> RuleSet<usize> {
        let rule = rule! {
            "integer (numeric)",
            (reg!(usize, r#"(\d{1,18})"#)),
            |text_match| Ok(text_match.group(0).parse::<usize>()?)
        };
        let rule_thousand = rule! {
            "integer (thousand)",
            (reg!(usize, "thousands?")),
            |_| Ok(1000usize)
        };
        let rule_compo = rule! {
            "number thousands",
            (   dim!(usize, vec!(Box::new(|a:&usize| *a > 1 && *a < 99))),
                dim!(usize, vec!(Box::new(|a:&usize| *a == 1000)))),
            |a,_| Ok(a.value()*1000)
        };
        let rule_set = RuleSet(vec![rule, rule_compo, rule_thousand]);
        rule_set
    }

    #[test]
    fn test_rule_set_application_all() {
        let rule_set = rules();
        let output_stash = rule_set.apply_all("foobar: 12 thousands").unwrap();
        assert_eq!(3, output_stash.len());
        let values: Vec<_> = output_stash.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![12, 1000, 12000], values);
    }

    #[test]
    fn test_integer_numeric_infix_rule() {
        let rule_int =
            rule! { "int", (reg!(usize, "\\d+")), |a| Ok(usize::from_str(&*a.group(0))?) };
        let rule_add = rule! {
            "add",
            (dim!(usize), reg!(usize, "\\+"), dim!(usize)),
            |a,_,b| Ok(a.value()+b.value())
        };
        let rule_mul = rule! {
            "mul",
            (dim!(usize), reg!(usize, "\\*"), dim!(usize)),
            |a,_,b| Ok(a.value()*b.value())
        };
        let rule_set = RuleSet(vec![rule_int, rule_add, rule_mul]);
        let results = rule_set.apply_all("foo: 12 + 42, 12* 42").unwrap();
        let values: Vec<_> = results.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![12, 42, 12, 42, 54, 504], values);
    }

    duckling_value! { Value
        UI(usize),
        FP(f32),
    }

    #[test]
    fn test_with_enum_value() {
        let int = rule! { "int", (reg!(Value, "\\d+")),
                |a| Ok(usize::from_str(&*a.group(0))?) };
        let fp = rule! { "fp", (reg!(Value, "\\d+.\\d+")),
                |a| Ok(f32::from_str(&*a.group(0))?) };
        let pow = rule! { "pow",
            (dim!(f32), reg!(Value, "\\^"), dim!(usize)),
           |a,_,b| Ok(a.value().powi(*b.value() as i32)) };
        let rule_set = RuleSet(vec![int, fp, pow]);
        let results = rule_set.apply_all("foo: 1.5^2").unwrap();
        let values: Vec<_> = results.into_iter().map(|pn| pn.value).collect();
        assert_eq!(vec![Value::UI(1), Value::UI(5), Value::UI(2), Value::FP(1.5), Value::FP(2.25)],
                   values);
    }
}
