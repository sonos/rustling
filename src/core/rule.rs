use std::fmt::Debug;

use core::*;
use core::pattern::*;

pub trait Rule<StashValue: Clone> {
    fn apply(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<ParsedNode<StashValue>>;
}

pub struct Rule1<A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<A, StashValue>,
          A: Match
{
    name: &'static str,
    pattern: PA,
    production: F,
    _phantom: ::std::marker::PhantomData<(V, A, StashValue)>,
}

impl<A, PA, V, StashValue, F> Rule<StashValue> for Rule1<A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<A, StashValue>,
          A: Match
{
    fn apply(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<ParsedNode<StashValue>> {
        let matches = self.matches(&stash, sentence);
        matches.iter()
            .filter_map(|sub| {
                let nodes = vec![sub.to_node()];
                if stash.iter().all(|old_node| {
                    old_node.root_node.children != nodes ||
                    old_node.root_node.rule_name != self.name
                }) {
                    Some(ParsedNode::new(self.name,
                                         StashValue::from((self.production)(sub)),
                                         sub.range(),
                                         vec![sub.to_node()]))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<A, PA, V, StashValue, F> Rule1<A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<A, StashValue>,
          A: Match
{
    pub fn new(name: &'static str, pat: PA, prod: F) -> Rule1<A, PA, V, StashValue, F> {
        Rule1 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<A> {
        self.pattern.predicate(stash, sentence)
    }
}

pub struct Rule2<A, B, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<A, StashValue>,
          A: Match,
          PB: Pattern<B, StashValue>,
          B: Match
{
    name: &'static str,
    pattern: (PA, PB),
    production: F,
    _phantom: ::std::marker::PhantomData<(V, A, B, StashValue)>,
}

impl<A, PA, B, PB, V, StashValue, F> Rule<StashValue> for Rule2<A, B, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<A, StashValue>,
          A: Match,
          PB: Pattern<B, StashValue>,
          B: Match
{
    fn apply(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<ParsedNode<StashValue>> {
        let matches = self.matches(&stash, sentence);
        matches.iter()
            .filter_map(|sub| {
                let nodes = vec![sub.0.to_node(), sub.1.to_node()];
                if stash.iter().all(|old_node| {
                    old_node.root_node.children != nodes ||
                    old_node.root_node.rule_name != self.name
                }) {
                    let range = (sub.0.range().0, sub.1.range().1);
                    Some(ParsedNode::new(self.name,
                                         (self.production)(&sub.0, &sub.1).into(),
                                         range,
                                         vec![sub.0.to_node(), sub.1.to_node()]))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<A, PA, B, PB, V, StashValue, F> Rule2<A, B, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<A, StashValue>,
          A: Match,
          PB: Pattern<B, StashValue>,
          B: Match
{
    pub fn new(name: &'static str,
               pat: (PA, PB),
               prod: F)
               -> Rule2<A, B, PA, PB, V, StashValue, F> {
        Rule2 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<(A, B)> {
        let matches_0 = self.pattern.0.predicate(stash, sentence);
        let matches_1 = self.pattern.1.predicate(stash, sentence);
        let mut result: Vec<(A, B)> = vec![];
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if m0.range().1 > m1.range().0 {
                    continue;
                }
                let spacing = &sentence[m0.range().1..m1.range().0];
                if !spacing.chars().all(|c| c.is_whitespace() || c == '-') {
                    continue;
                }
                result.push((m0.clone(), m1.clone()))
            }
        }
        result
    }
}


#[cfg(test)]
mod tests {
    #[macro_use]
    use core::*;
    use core::pattern::*;
    use core::rule::*;
    use core::test_helpers::*;

    #[test]
    fn test_integer_numeric_en_rule() {
        let rule = rule! { "ten", (reg!(usize, "ten")), |_| 10usize };
        assert_eq!(vec![Text(vec!["ten".into()], (8, 11), "ten")],
                   rule.matches(&vec![], "foobar: ten"));
        assert_eq!(vec![Text(vec!["ten".into()], (8, 11), "ten"),
                        Text(vec!["ten".into()], (12, 15), "ten")],
                   rule.matches(&vec![], "foobar: ten ten"));
        assert_eq!(vec![ParsedNode::new("ten",
                                        10,
                                        (8, 11),
                                        vec![Node::new("ten", (8, 11), vec![])]),
                        ParsedNode::new("ten",
                                        10,
                                        (12, 15),
                                        vec![Node::new("ten", (12, 15), vec![])])],
                   rule.apply(&vec![], "foobar: ten ten"))
    }

    #[test]
    fn test_integer_numeric_compo_en_rule() {
        let rule_consec = rule! {
            "2 consecutive ints",
            (dim!(usize), dim!(usize, vec![|integer: &usize| *integer == 10])),
            |a,b| a.value+b.value
        };
        let stash: Stash<usize> = vec![ParsedNode::new("ten", 10, (8, 11), vec![]),
                                       ParsedNode::new("ten", 10, (12, 15), vec![])];
        assert_eq!(vec![(stash[0].clone(), stash[1].clone())],
                   rule_consec.matches(&stash, "foobar: ten ten"));
        assert_eq!(vec![ParsedNode::new("2 consecutive ints",
                                        20,
                                        (8, 15),
                                        vec![stash[0].root_node.clone(),
                                             stash[1].root_node.clone()])],
                   rule_consec.apply(&stash, "foobar: ten ten"));
    }
}
