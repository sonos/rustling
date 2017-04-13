#![allow(dead_code)]

extern crate regex;

type Range = (usize, usize);

#[derive(Clone,Debug)]
enum GValue {
    USize(usize),
    String(String),
}

impl From<usize> for GValue {
    fn from(a:usize) -> GValue {
        GValue::USize(a)
    }
}

impl From<String> for GValue {
    fn from(a:String) -> GValue {
        GValue::String(a)
    }
}

    /*
impl Value for GValue {
    fn generic(&self) -> GValue {
        self.clone()
    }
}
    */

trait Match: Clone {
    fn range(&self) -> Range;
    fn to_node(&self) -> Node;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub rule_name: &'static str,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Clone,Debug)]
struct Text(Vec<String>, Range, &'static str);
impl Match for Text {
    fn range(&self) -> Range {
        self.1
    }

    fn to_node(&self) -> Node {
        Node {
            rule_name: self.2,
            range: self.range(),
            children: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct ParsedNode<V: Clone> {
    pub root_node: Node,
    pub value: V,
    pub latent: bool,
}

impl<V: Clone> Match for ParsedNode<V> {
    fn range(&self) -> Range {
        self.root_node.range
    }

    fn to_node(&self) -> Node {
        self.root_node.clone()
    }
}

impl<V:Clone> ParsedNode<V> {
    fn new(name: &'static str, v: V, r: Range, children: Vec<Node>) -> ParsedNode<V> {
        ParsedNode {
            root_node: Node {
                rule_name: name,
                range: r,
                children: children,
            },
            value: v,
            latent: false,
        }
    }
}

// #[derive(Clone,Debug)]
// struct ParsedNode<V:Clone>(V, Range);
//

type Stash<V> = Vec<ParsedNode<V>>;

trait Pattern<M: Match, StashValue: Clone> {
    fn predicate(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<M>;
}

trait Rule<StashValue: Clone> {
    fn apply(&self, stash: &Stash<StashValue>, sentence: &str) -> Vec<ParsedNode<StashValue>>;
}

struct Rule1<A, PA, V, StashValue, F>
    where V:Clone,
          StashValue: From<V>+Clone,
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
    where V:Clone,
          StashValue: From<V>+Clone,
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
    where V:Clone,
          StashValue: From<V>+Clone,
          F: Fn(&A) -> V,
          PA: Pattern<A, StashValue>,
          A: Match
{
    fn new(name: &'static str, pat: PA, prod: F) -> Rule1<A, PA, V, StashValue, F> {
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

struct Rule2<A, B, PA, PB, V, StashValue, F>
    where V:Clone,
          StashValue: From<V>+Clone,
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
    where V:Clone,
          StashValue: From<V>+Clone,
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
    where V:Clone,
          StashValue: From<V>+Clone,
          F: Fn(&A, &B) -> V,
          PA: Pattern<A, StashValue>,
          A: Match,
          PB: Pattern<B, StashValue>,
          B: Match
{
    fn new(name: &'static str, pat: (PA, PB), prod: F) -> Rule2<A, B, PA, PB, V, StashValue, F> {
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

struct RuleSet<StashValue: Clone>(Vec<Box<Rule<StashValue>>>);

impl<StashValue: Clone> RuleSet<StashValue> {
    fn apply_once(&self, stash: &mut Stash<StashValue>, sentence: &str) {
        let produced_nodes: Vec<ParsedNode<StashValue>> = self.0
            .iter()
            .flat_map(|rule| rule.apply(stash, sentence).into_iter())
            .collect();
        stash.extend(produced_nodes)
    }

    fn apply(&self, sentence: &str) -> Stash<StashValue> {
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

struct TextPattern<StashValue: Clone>(::regex::Regex, &'static str, ::std::marker::PhantomData<StashValue>);

impl<StashValue: Clone> TextPattern<StashValue> {
	fn new(regex: ::regex::Regex, name: &'static str) -> TextPattern<StashValue> {
		TextPattern(regex, name, ::std::marker::PhantomData)
	}
}

impl<StashValue: Clone> Pattern<Text, StashValue> for TextPattern<StashValue> {
    fn predicate(&self, _stash: &Stash<StashValue>, sentence: &str) -> Vec<Text> {
        self.0
            .captures_iter(&sentence)
            .map(|cap| {
                let full = cap.get(0).unwrap();
                let full_range = (full.start(), full.end());
                Text(cap.iter().map(|m| m.unwrap().as_str().to_string()).collect(),
                     full_range,
                     self.1)
            })
            .collect()
    }
}

// struct ParsedNodePattern;
//
// impl Pattern<ParsedNode<usize>> for ParsedNodePattern {
// fn predicate(&self, stash:&Stash<GValue>, sentence:&str) -> Vec<ParsedNode<usize>> {
// unimplemented!()
// }
// }
//

struct UsizeNodePattern;

impl Pattern<ParsedNode<usize>, GValue> for UsizeNodePattern {
    fn predicate(&self, stash: &Stash<GValue>, _sentence: &str) -> Vec<ParsedNode<usize>> {
        stash.iter()
            .filter_map(|it| if let GValue::USize(v) = it.value {
                Some(ParsedNode::new(it.root_node.rule_name,
                                     v,
                                     it.range(),
                                     it.root_node.children.clone()))
            } else {
                None
            })
            .collect()
    }
}


type AnyNodePattern<V> = DimensionNodePattern<V, fn(&V) -> bool>;

trait AttemptFrom<V>: Sized {
	fn attempt_from(v: V) -> Option<Self>;
}

impl AttemptFrom<GValue> for usize {
	fn attempt_from(v: GValue) -> Option<usize> {
		if let GValue::USize(value) = v {
			Some(value)
		} else {
			None
		}
	}
}

impl AttemptFrom<GValue> for String {
	fn attempt_from(v: GValue) -> Option<String> {
		if let GValue::String(value) = v {
			Some(value)
		} else {
			None
		}
	}
}

struct DimensionNodePattern<V, F>
	where V: Clone,
		  F: Fn(&V) -> bool, 
	{
	predicates: Vec<F>,
	_phantom: ::std::marker::PhantomData<V>,
}

impl<V: Clone> AnyNodePattern<V> {
	fn new() -> AnyNodePattern<V> {
		DimensionNodePattern {
			predicates: vec![],
			_phantom: ::std::marker::PhantomData
		}
	}
}

impl<V, F> DimensionNodePattern<V, F> 
	where V: Clone,
		  F: Fn(&V) -> bool,
	{
	fn filter(predicates: Vec<F>) -> DimensionNodePattern<V, F> {
		DimensionNodePattern {
			predicates: predicates,
			_phantom: ::std::marker::PhantomData
		}
	}

}

impl<StashValue, V, F> Pattern<ParsedNode<V>, StashValue> for DimensionNodePattern<V, F>
	where StashValue: Clone,
		  V: AttemptFrom<StashValue>+Clone,
		  F: Fn(&V) -> bool ,
	{
	fn predicate(&self, stash: &Stash<StashValue>, _sentence: &str) -> Vec<ParsedNode<V>> {
		stash.iter()
            .filter_map(|it| if let Some(v) = V::attempt_from(it.value.clone()) {
            	if self.predicates.iter().all(|predicate| (predicate)(&v)) {
            		Some(ParsedNode::new(it.root_node.rule_name,
                                     v,
                                     it.range(),
                                     it.root_node.children.clone()))
            	} else {
            		None
            	}
            } else {
                None
            })
            .collect()
	}
}


macro_rules! reg {
    ($pattern:expr) => ( TextPattern::<GValue>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
}

macro_rules! rule {
    ($name:expr, ($a:expr), $f:expr) => { Box::new(Rule1::new($name, $a, $f)) };
    ($name:expr, ($a:expr, $b:expr), $f:expr) => { Box::new(Rule2::new($name, ($a, $b), $f)) };
//    (($a:expr, $b:expr, $c:expr), $f:expr) => { Box::new(Rule3::new(($a, $b, $c), $f)) };
}

macro_rules! dim {
	($typ:ty) => ( AnyNodePattern::<$typ>::new() );
	($typ:ty, $predicates:expr) => ( DimensionNodePattern::<$typ,_>::filter($predicates) );
}

#[test]
fn test_rule_1() {
    let rule_ten = rule! {
        "ten",
        (reg!(r#"ten"#)),
        |_| 10
    };
    println!("{:?}", rule_ten.apply(&vec![], "foubar ten"));
}

#[test]
fn test_rule_2() {
    let rule_consec = rule! {
        "2 consecutive ints",
        (dim!(usize), dim!(usize, vec![|integer: &usize| *integer == 10])),
        |a,b| a.value+b.value
    };
    let stash = vec![ParsedNode::new("ten", 10.into(), (7, 10), vec![]),
                     ParsedNode::new("ten", 10.into(), (11, 14), vec![])];
    println!("{:?}", rule_consec.apply(&stash, "foubar ten ten"));
}

#[test]
fn test_rule_set() {
    let rule_ten = rule! {
        "ten",
        (reg!(r#"ten"#)),
        |_| 10
    };

    let rule_consec = rule! {
        "2 consecutive ints",
        (dim!(usize), dim!(usize)),
        |a,b| a.value+b.value
    };
    let ruleset = RuleSet(vec![rule_ten, rule_consec]);
    for sub in ruleset.apply("foobar ten ten") {
        println!("{:?}",  sub);
    }
}

// mod try3 {
// use super::*;
//
// trait Rule<V:Clone> {
// fn apply(&self, stash: &Stash<V>, sentence: &str) -> Vec<ParsedNode<V>>;
// }
// struct RuleSet<V:Clone>(Vec<Box<Rule<V>>>);
//
// macro_rules! R {
// ($name:ident, ($(($m:ident,$p:ident)),*)) => {
// #[allow(dead_code)]
// struct $name<V, F, $($m),*, $($p),*>
// where   V:Clone,
// $($m: Match),*,
// $($p: Pattern<$m>),*,
// F: Fn(($($m),*)) -> V
// {
// pattern: ($($p),*),
// production: F,
// _phantom: ::std::marker::PhantomData<(V, $($m),*)>,
// }
//
// impl<V, F, $($m),*, $($p),*> $name<V,F, $($m),*, $($p),*>
// where   V:Clone,
// $($m: Match),*,
// $($p: Pattern<$m>),*,
// F: Fn(($($m),*)) -> V
// {
// fn new(pats:(($($p),*)),f:F) -> $name<V, F, $($m),*, $($p),*> {
// $name {
// pattern: pats,
// production: f,
// _phantom: ::std::marker::PhantomData,
// }
// }
// #[allow(dead_code)]
// fn matches(&self, _stash: &Stash<V>, _sentence:&str) -> Vec<($($m),*)> {
// unimplemented!()
// }
// #[allow(dead_code)]
// fn rec_matches(&self) -> Vec<($($m),*)> {
// unimplemented!()
// }
// #[allow(dead_code)]
// fn produce(&self, matches: ($($m),*)) -> ParsedNode<V> {
// ParsedNode((self.production)(matches))
// }
// }
//
// impl<V, F, $($m),*, $($p),*> Rule<V> for $name<V,F, $($m),*, $($p),*>
// where   V:Clone,
// $($m: Match),*,
// $($p: Pattern<$m>),*,
// F: Fn(($($m),*)) -> V
// {
// fn apply(&self, s