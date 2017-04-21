use duckling::Value;
use duckling::ParsedNode;
use super::*;
use core::RuleSet;
use ml::Model;

// DUCKLING
#[derive(Debug)]
pub struct Example<V: Value> {
    text: &'static str,
    predicate: Box<Check<V>>,
    _phantom: ::std::marker::PhantomData<V>,
}

impl<V: Value> Example<V> {
    pub fn new(text: &'static str, predicate: Box<Check<V>>) -> Example<V> {
        Example {
            text: text,
            predicate: predicate,
            _phantom: ::std::marker::PhantomData,
        }
    }
}

trait Check<V: Value>: ::std::fmt::Debug {
    fn check(&self, &ParsedNode<V>) -> bool;
}

pub fn train(rules: &RuleSet<Dimension>,
         examples: &Vec<Example<Dimension>>)
         -> Model<duckling::Id, duckling::Class, parser::Feat> {
    use std::collections::HashMap;
    use std::collections::HashSet;
    use duckling::FeatureExtractor;
    use examples::Check;
    use core::Node;
    let mut classified_ex: HashMap<duckling::Id,
                                   Vec<(HashMap<parser::Feat, usize>, duckling::Class)>> =
        HashMap::new();
    for ex in examples {
        let mut true_exs = vec!();
        let mut false_exs = vec!();
        let stash = rules.apply_all(&ex.text).unwrap();
        for candidate in stash {
            if !(candidate.root_node.range.0 == 0 && candidate.root_node.range.1 == ex.text.len()) {
                continue;
            }
            if ex.predicate.check(&candidate) {
                true_exs.push(candidate);
            } else {
                false_exs.push(candidate);
            }
        }
        let mut false_nodes = HashSet::new();
        let mut true_nodes = HashSet::new();
        fn add_to_set(nodes:&mut HashSet<Node>, node: &Node) {
            nodes.insert(node.clone());
            for child in &node.children {
                add_to_set(nodes, child);
            }
        }
        for parse_node in true_exs {
            add_to_set(&mut true_nodes, &parse_node.root_node);
        }
        for parse_node in false_exs {
            add_to_set(&mut false_nodes, &parse_node.root_node);
        }
        false_nodes = &false_nodes - &true_nodes;
        for n in true_nodes {
            let mut counted_features = HashMap::new();
            for f in parser::extract_node_features(&n).features {
                *counted_features.entry(f).or_insert(0) += 1;
            }
            classified_ex.entry(duckling::Id(n.rule_name)).or_insert(vec!()).push((counted_features, duckling::Class(true)));
        }
        for n in false_nodes {
            let mut counted_features = HashMap::new();
            for f in parser::extract_node_features(&n).features {
                *counted_features.entry(f).or_insert(0) += 1;
            }
            classified_ex.entry(duckling::Id(n.rule_name)).or_insert(vec!()).push((counted_features, duckling::Class(false)));
        }
    }
    let classifiers = classified_ex.into_iter().map(|(id,examples)| (id, ml::Classifier::train(&examples))).collect();
    Model {
        classifiers: classifiers
    }
}

// ONTOLOGY
#[derive(Debug)]
struct CheckInteger {
    value: i64,
}

impl Check<Dimension> for CheckInteger {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        IntegerValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_integer(v: i64) -> CheckInteger {
    CheckInteger { value: v }
}

#[derive(Debug)]
struct CheckOrdinal {
    value: i64,
}

impl Check<Dimension> for CheckOrdinal {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        OrdinalValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_ordinal(v: i64) -> CheckOrdinal {
    CheckOrdinal { value: v }
}

#[derive(Debug)]
struct CheckFloat {
    value: f32,
}

impl Check<Dimension> for CheckFloat {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        FloatValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_float(v: f32) -> CheckFloat {
    CheckFloat { value: v }
}

macro_rules! example {
    ($v:expr, $check:expr, $($ex:expr),*) => {
        $( $v.push(Example::new($ex, Box::new($check))); )*
    };
}

pub fn examples_en_numbers() -> Vec<Example<Dimension>> {
    let mut v = vec![];
    example!(v, check_integer(0), "0", "naught", "nought", "zero", "nil");
    example!(v, check_integer(1), "1", "one", "single");
    example!(v, check_integer(2), "2", "two", "a pair");
    example!(v, check_integer(33), "33", "thirty three", "0033");
    example!(v, check_integer(14), "14", "fourteen");
    example!(v, check_integer(16), "16", "sixteen");
    example!(v, check_integer(17), "17", "seventeen");
    example!(v, check_integer(18), "18", "eighteen");
    example!(v, check_float(1.1), "1.1", "1.10", "01.10");
    example!(v, check_float(0.77), "0.77", ".77");
    example!(v,
             check_integer(100000),
             "100,000",
             "100000",
             "100K",
             "100k");
    example!(v,
             check_integer(3000000),
             "3M",
             "3000K",
             "3000000",
             "3,000,000");
    example!(v,
             check_integer(1200000),
             "1,200,000",
             "1200000",
             "1.2M",
             "1200K",
             ".0012G");
    example!(v,
             check_integer(-1200000),
             "- 1,200,000",
             "-1200000",
             "minus 1,200,000",
             "negative 1200000",
             "-1.2M",
             "-1200K",
             "-.0012G");
    example!(v, check_integer(5000), "5 thousand", "five thousand");
    example!(v, check_integer(122), "one twenty two");
    example!(v, check_integer(200000), "two hundred thousand");
    example!(v, check_integer(21011), "twenty-one thousand eleven");
    example!(v,
             check_integer(721012),
             "seven hundred twenty-one thousand twelve",
             "seven hundred twenty-one thousand and twelve");
    example!(v,
             check_integer(31256721),
             "thirty-one million two hundred fifty-six thousand seven hundred twenty-one");
    example!(v, check_ordinal(4), "the 4th", "4th", "fourth");
    example!(v, check_ordinal(3), "the 3rd", "3rd", "third");
    example!(v, check_ordinal(2), "the 2nd", "2nd", "second");
    example!(v, check_ordinal(21), "the twenty first");
    v
}
