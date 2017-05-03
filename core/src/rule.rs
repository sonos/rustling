use ::*;
use pattern::*;
use errors::*;
use rule::rule_errors::*;
use smallvec::SmallVec;

pub mod rule_errors {
    error_chain! {
        types {
            RuleError, RuleErrorKind, RuleResultExt, RuleResult;
        }

        foreign_links {
            NumParseInt(::std::num::ParseIntError);
            NumParseFloat(::std::num::ParseFloatError);
        }

        errors {
            Invalid
        }
    }
}

fn make_production_error(s: RuleError) -> CoreError {
    CoreErrorKind::ProductionRuleError(format!("{:?}", s)).into()

}

macro_rules! svec {
    ($($item:expr),*) => { {
        let mut v = ::smallvec::SmallVec::new();
        $( v.push($item); )*
        v
    }
    }
}

pub struct RuleProductionArg<'a, M: Match + 'a> {
    sentence: &'a str,
    match_: &'a M,
}

impl<'a, M: Match> RuleProductionArg<'a, M> {
    pub fn new(sentence: &'a str, match_: &'a M) -> RuleProductionArg<'a, M> {
        RuleProductionArg {
            sentence: sentence,
            match_: match_,
        }
    }
}

impl<'a> RuleProductionArg<'a, Text> {
    pub fn group(&self, ix: usize) -> &'a str {
        let g = self.match_.groups[ix];
        &self.sentence[g.0..g.1]
    }
}

impl<'a, V: Clone> RuleProductionArg<'a, ParsedNode<V>> {
    pub fn value(&self) -> &V {
        &self.match_.value
    }
}

type ParsedNodes<StashValue> = SmallVec<[ParsedNode<StashValue>; 1]>;

pub trait Rule<StashValue: Clone + Send + Sync>: Send + Sync {
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>>;
}

pub struct Rule1<PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>
{
    name: &'static str,
    pattern: PA,
    production: F,
    _phantom: ::std::marker::PhantomData<(V, StashValue)>,
}

impl<PA, V, StashValue, F> Rule<StashValue> for Rule1<PA, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec![sub.to_node()];
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_name != self.name
                            }) {
                    match (self.production)(&RuleProductionArg::new(sentence, sub)) {
                        Ok(v) => {
                            Some(Ok(ParsedNode::new(self.name,
                                                    StashValue::from(v),
                                                    sub.range(),
                                                    nodes)))
                        }
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<PA, V, StashValue, F> Rule1<PA, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>
{
    pub fn new(name: &'static str, pat: PA, prod: F) -> Rule1<PA, V, StashValue, F> {
        Rule1 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<PA::M>> {
        self.pattern.predicate(stash, sentence)
    }
}

fn adjacent<A: Match, B: Match>(a: &A, b: &B, sentence: &str) -> bool {
    a.range().1 <= b.range().0 &&
    sentence[a.range().1..b.range().0]
        .chars()
        .all(|c| c.is_whitespace() || c == '-')
}

pub struct Rule2<PA, PB, V, StashValue, F>
    where V: Clone,
          StashValue: From<V> + Clone + Send + Sync,
          F:for<'a>  Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    name: &'static str,
    pattern: (PA, PB),
    production: F,
    _phantom: ::std::marker::PhantomData<(V,  StashValue)>,
}

impl<PA, PB, V, StashValue, F> Rule<StashValue>
    for Rule2<PA, PB, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches.iter()
            .filter_map(|sub| {
                let nodes = svec![sub.0.to_node(), sub.1.to_node()];
                if stash.iter().all(|old_node| {
                    old_node.root_node.children != nodes ||
                    old_node.root_node.rule_name != self.name
                }) {
                    let range = Range(sub.0.range().0, sub.1.range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0), &RuleProductionArg::new(sentence, &sub.1)) {
                        Ok(v) => {
                            Some(Ok(ParsedNode::new(self.name,
                                                    v.into(),
                                                    range,
                                                    nodes)))
                        }
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<PA, PB, V, StashValue, F> Rule2<PA, PB, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    pub fn new(name: &'static str,
               pat: (PA, PB),
               prod: F)
               -> Rule2<PA, PB, V, StashValue, F> {
        Rule2 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &str) -> CoreResult<PredicateMatches<(PA::M, PB::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result)
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    result.push((m0.clone(), m1.clone()))
                }
            }
        }
        Ok(result)
    }
}

pub struct Rule3<PA, PB, PC, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>
{
    name: &'static str,
    pattern: (PA, PB, PC),
    production: F,
    _phantom: ::std::marker::PhantomData<(V, StashValue)>,
}

impl<PA, PB, PC, V, StashValue, F> Rule<StashValue> for Rule3<PA, PB, PC, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec!(sub.0.to_node(), sub.1.to_node());
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_name != self.name
                            }) {
                    let range = Range(sub.0.range().0, sub.2.range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0),
                                            &RuleProductionArg::new(sentence, &sub.1),
                                            &RuleProductionArg::new(sentence, &sub.2)) {
                        Ok(v) => Some(Ok(ParsedNode::new(self.name, v.into(), range, nodes))),
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<PA, PB, PC, V, StashValue, F> Rule3<PA, PB, PC, V, StashValue, F>
    where V: Clone + Send + Sync,
          StashValue: From<V> + Clone + Send + Sync,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>
{
    pub fn new(name: &'static str,
               pat: (PA, PB, PC),
               prod: F)
               -> Rule3<PA, PB, PC, V, StashValue, F> {
        Rule3 {
            name: name,
            pattern: pat,
            production: prod,
            _phantom: ::std::marker::PhantomData,
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<(PA::M, PB::M, PC::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result);
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        if matches_1.is_empty() {
            return Ok(result);
        }
        let matches_2 = self.pattern.2.predicate(stash, sentence)?;
        if matches_2.is_empty() {
            return Ok(result);
        }
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
        Ok(result)
    }
}


#[cfg(test)]
mod tests {
    use rule::*;

    macro_rules! svec {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::new();
            $( v.push($item); )*
            v
        }
        }
    }

    impl AttemptFrom<usize> for usize {
        fn attempt_from(v: usize) -> Option<usize> {
            Some(v)
        }
    }

    macro_rules! reg {
        ($typ:ty, $pattern:expr) => ( $crate::pattern::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
    }

    #[test]
    fn test_integer_numeric_en_rule() {
        let rule = Rule1::new("ten", (reg!(usize, "ten")), |_| Ok(10usize));
        assert_eq!(vec![Text::new(svec![Range(8, 11)], Range(8, 11), "ten")],
                   rule.matches(&vec![], "foobar: ten").unwrap());
        assert_eq!(vec![Text::new(svec![Range(8, 11)], Range(8, 11), "ten"),
                        Text::new(svec![Range(12, 15)], Range(12, 15), "ten")],
                   rule.matches(&vec![], "foobar: ten ten").unwrap());
        assert_eq!(vec![ParsedNode::new("ten",
                                        10,
                                        Range(8, 11),
                                        vec![Node::new("ten", Range(8, 11), vec![])]),
                        ParsedNode::new("ten",
                                        10,
                                        Range(12, 15),
                                        vec![Node::new("ten", Range(12, 15), vec![])])],
                   rule.apply(&vec![], "foobar: ten ten").unwrap())
    }

    #[test]
    fn test_integer_numeric_compo_en_rule() {
        let rule_consec =
            Rule2::new("2 consecutive ints",
                       (AnyNodePattern::<usize>::new(),
                        FilterNodePattern::<usize>::filter(vec![Box::new(|integer: &usize| {
                                                                             *integer == 10
                                                                         })])),
                       |a, b| Ok(a.value() + b.value()));
        let stash: Stash<usize> = vec![ParsedNode::new("ten", 10, Range(8, 11), vec![]),
                                       ParsedNode::new("ten", 10, Range(12, 15), vec![])];
        assert_eq!(vec![(stash[0].clone(), stash[1].clone())],
                   rule_consec.matches(&stash, "foobar: ten ten").unwrap());
        assert_eq!(vec![ParsedNode::new("2 consecutive ints",
                                        20,
                                        Range(8, 15),
                                        vec![stash[0].root_node.clone(),
                                             stash[1].root_node.clone()])],
                   rule_consec.apply(&stash, "foobar: ten ten").unwrap());
    }

    #[test]
    fn test_integer_numeric_int_rule() {
        use std::str::FromStr;
        let rule_int = Rule1::new("int",
                                  (reg!(usize, "\\d+")),
                                  |a| Ok(usize::from_str(&*a.group(0))?));
        assert_eq!(vec![ParsedNode::new("int",
                                        42,
                                        Range(8, 10),
                                        vec![Node::new("\\d+", Range(8, 10), vec![])])],
                   rule_int.apply(&vec![], "foobar: 42").unwrap());
    }

}
