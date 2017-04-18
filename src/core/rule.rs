use core::*;
use core::pattern::*;

pub trait Rule<'a, StashValue: Clone> {
    fn apply(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<ParsedNode<StashValue>>;
}

pub struct Rule1<'a, A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a> + 'a
{
    name: &'static str,
    pattern: PA,
    production: F,
    _phantom: ::std::marker::PhantomData<(V, A, StashValue, &'a ())>,
}

impl<'a, A, PA, V, StashValue, F> Rule<'a, StashValue> for Rule1<'a, A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a> + 'a
{
    fn apply(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<ParsedNode<StashValue>> {
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

impl<'a, A, PA, V, StashValue, F> Rule1<'a, A, PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>
{
    pub fn new(name: &'static str, pat: PA, prod: F) -> Rule1<'a, A, PA, V, StashValue, F> {
        Rule1 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<A> {
        self.pattern.predicate(stash, sentence)
    }
}

fn adjacent<'a, A: Match<'a>, B: Match<'a>>(a: &A, b: &B, sentence: &'a str) -> bool {
    a.range().1 <= b.range().0 &&
    sentence[a.range().1..b.range().0].chars().all(|c| c.is_whitespace() || c == '-')
}

pub struct Rule2<'a, A: 'a, B: 'a, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>
{
    name: &'static str,
    pattern: (PA, PB),
    production: F,
    _phantom: ::std::marker::PhantomData<(V, A, B, StashValue, &'a ())>,
}

impl<'a, A, PA, B, PB, V, StashValue, F> Rule<'a, StashValue>
    for Rule2<'a, A, B, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>
{
    fn apply(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<ParsedNode<StashValue>> {
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

impl<'a, A, PA, B, PB, V, StashValue, F> Rule2<'a, A, B, PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>
{
    pub fn new(name: &'static str,
               pat: (PA, PB),
               prod: F)
               -> Rule2<'a, A, B, PA, PB, V, StashValue, F> {
        Rule2 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<(A, B)> {
        let matches_0 = self.pattern.0.predicate(stash, sentence);
        let matches_1 = self.pattern.1.predicate(stash, sentence);
        let mut result: Vec<(A, B)> = vec![];
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    result.push((m0.clone(), m1.clone()))
                }
            }
        }
        result
    }
}

pub struct Rule3<'a, A: 'a, B: 'a, C: 'a, PA, PB, PC, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&'a A, &'a B, &'a C) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>,
          PC: Pattern<'a, C, StashValue>,
          C: Match<'a>
{
    name: &'static str,
    pattern: (PA, PB, PC),
    production: F,
    _phantom: ::std::marker::PhantomData<(V, A, B, C, StashValue, &'a ())>,
}

impl<'a, A, PA, B, PB, C, PC, V, StashValue, F> Rule<'a, StashValue>
    for Rule3<'a, A, B, C, PA, PB, PC, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B, &C) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>,
          PC: Pattern<'a, C, StashValue>,
          C: Match<'a>
{
    fn apply(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<ParsedNode<StashValue>> {
        let matches = self.matches(&stash, sentence);
        matches.iter()
            .filter_map(|sub| {
                let nodes = vec![sub.0.to_node(), sub.1.to_node()];
                if stash.iter().all(|old_node| {
                    old_node.root_node.children != nodes ||
                    old_node.root_node.rule_name != self.name
                }) {
                    let range = (sub.0.range().0, sub.2.range().1);
                    Some(ParsedNode::new(self.name,
                                         (self.production)(&sub.0, &sub.1, &sub.2).into(),
                                         range,
                                         nodes))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<'a, A, PA, B, PB, C, PC, V, StashValue, F> Rule3<'a, A, B, C, PA, PB, PC, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone,
          F: Fn(&A, &B, &C) -> V,
          PA: Pattern<'a, A, StashValue>,
          A: Match<'a>,
          PB: Pattern<'a, B, StashValue>,
          B: Match<'a>,
          PC: Pattern<'a, C, StashValue>,
          C: Match<'a>
{
    pub fn new(name: &'static str,
               pat: (PA, PB, PC),
               prod: F)
               -> Rule3<'a, A, B, C, PA, PB, PC, V, StashValue, F> {
        Rule3 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<(A, B, C)> {
        let matches_0 = self.pattern.0.predicate(stash, sentence);
        let matches_1 = self.pattern.1.predicate(stash, sentence);
        let matches_2 = self.pattern.2.predicate(stash, sentence);
        let mut result: Vec<(A, B, C)> = vec![];
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    for m2 in matches_2.iter() {
                        if adjacent(m1, m2, sentence) {
                            result.push((m0.clone(), m1.clone(), m2.clone()))
                        }
                    }
                }
            }
        }
        result
    }
}


#[cfg(test)]
mod tests {
    use core::rule::*;

    macro_rules! svec {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::new();
            $( v.push($item); )*
            v
        }
        }
    }

    #[test]
    fn test_integer_numeric_en_rule() {
        let rule = rule! { "ten", (reg!(usize, "ten")), |_| 10usize };
        assert_eq!(vec![Text(svec!["ten".into()], (8, 11), "ten")],
                   rule.matches(&vec![], "foobar: ten"));
        assert_eq!(vec![Text(svec!["ten".into()], (8, 11), "ten"),
                        Text(svec!["ten".into()], (12, 15), "ten")],
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
            (dim!(usize), dim!(usize, vec![Box::new(|integer: &usize| *integer == 10)])),
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

    #[test]
    fn test_integer_numeric_int_rule() {
        use std::str::FromStr;
        let rule_int = rule! { "int", (reg!(usize, "\\d+")),
            |a| usize::from_str(&*a.0[0]).unwrap() };
        assert_eq!(vec![ParsedNode::new("int",
                                        42,
                                        (8, 10),
                                        vec![Node::new("\\d+", (8, 10), vec![])])],
                   rule_int.apply(&vec![], "foobar: 42"));
    }

}
