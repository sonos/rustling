#[macro_use]
extern crate error_chain;
extern crate duckling_core;
extern crate duckling_ml;

use ::std::cmp::{PartialOrd, Ordering};

pub use duckling_core::regex;
pub use duckling_core::{AttemptFrom, Node, ParsedNode, Range, RuleSet};
pub use duckling_core::{RuleError, RuleErrorKind, RuleResult};
pub use duckling_ml::{ ClassId, Classifier, ClassifierId, Feature, Input, Model };
pub use train::{ Check, Example };
pub use errors::*;

#[macro_use]
pub mod macros;
pub mod train;

pub mod core {
    pub use duckling_core::pattern::{ AnyNodePattern, FilterNodePattern, TextNegLHPattern, TextPattern };
    pub use duckling_core::rule::{ Rule1, Rule2, Rule3 };
}

pub mod errors {
    error_chain! {
        types {
            DucklingError, DucklingErrorKind, DucklingResultExt, DucklingResult;
        }
        links {
            Core(::duckling_core::errors::CoreError, ::duckling_core::errors::CoreErrorKind);
            ML(::duckling_ml::errors::MLError, ::duckling_ml::errors::MLErrorKind);
        }
        foreign_links {
            Regex(::regex::Error);
        }
        errors {
            ProductionRuleError(t: String)
        }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct RuleId(pub &'static str);
impl ClassifierId for RuleId {}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Truth(pub bool);
impl ClassId for Truth {}

pub trait Value: Clone + PartialEq + ::std::fmt::Debug + ::std::fmt::Display {
    fn same_dimension_as(&self, other: &Self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserMatch<V: Value> {
    pub range: Range,
    pub value: V,
    pub probalog: f32,
}

fn match_cmp<V>(a: &(ParsedNode<V>, ParserMatch<V>, Option<usize>),
                b: &(ParsedNode<V>, ParserMatch<V>, Option<usize>))
                -> Option<Ordering>
    where V: Value
{
    if a.1.value.same_dimension_as(&b.1.value) {
        if a.1.range == b.1.range {
            a.1.probalog.partial_cmp(&b.1.probalog)
        } else if a.1.range.intersects(&b.1.range) {
            b.1.range.1.partial_cmp(&a.1.range.1)
        } else {
            a.1.range.partial_cmp(&b.1.range)
        }
    } else {
        if (a.1.range == b.1.range || a.1.range.intersects(&b.1.range)) &&
           (a.2.is_some() && b.2.is_some()) {
            a.2.unwrap().partial_cmp(&b.2.unwrap()) // checked
        } else {
            None
        }
    }
}

pub trait FeatureExtractor<V: Value, Feat: Feature> {
    fn for_parsed_node(&self, node:&ParsedNode<V>) -> Input<RuleId, Feat>;
    fn for_node(&self, node:&Node) -> Input<RuleId, Feat>;
}

pub struct Parser<V: Value, Feat: Feature, Extractor: FeatureExtractor<V, Feat>> {
    rules: RuleSet<V>,
    model: Model<RuleId, Truth, Feat>,
    extractor: Extractor,
}

impl<V, Feat, Extractor> Parser<V, Feat, Extractor>
    where V: Value,
          RuleId: ClassifierId,
          Feat: Feature,
          Extractor: FeatureExtractor<V, Feat>
{
    pub fn new(rules: RuleSet<V>,
               model: Model<RuleId, Truth, Feat>,
               extractor: Extractor
               )
               -> Parser<V, Feat, Extractor> {
        Parser {
            rules: rules,
            model: model,
            extractor: extractor
        }
    }

    fn raw_candidates(&self, input: &str) -> DucklingResult<Vec<(ParsedNode<V>, ParserMatch<V>)>> {
        self.rules
            .apply_all(input)?
            .into_iter()
            .map(|p| {
                let features: Input<RuleId, Feat> = self.extractor.for_parsed_node(&p);
                let probalog = self.model.classify(&features, &Truth(true))?;
                let pm = ParserMatch {
                        range: p.root_node.range,
                        value: p.value.clone(),
                        probalog: probalog };
                Ok((p, pm))
            })
            .collect()
    }

    pub fn candidates<S: Fn(&V) -> Option<usize>>
        (&self,
         input: &str,
         dimension_prio: S)
         -> DucklingResult<Vec<(ParsedNode<V>, ParserMatch<V>, Option<usize>, bool)>> {
        let candidates = self.raw_candidates(input)?
            .into_iter()
            .map(|(pn, pm)| {
                let p = dimension_prio(&pm.value);
                (pn, pm, p)
            })
            .collect();
        Ok(tag_maximal_elements(candidates, |a, b| match_cmp(a, b))
            .into_iter()
            .map(|((pn,pv,prio), tag)| (pn, pv, prio, tag))
            .collect())
    }

    pub fn parse<S: Fn(&V) -> Option<usize>>(&self,
                                             input: &str,
                                             dimension_prio: S)
                                             -> DucklingResult<Vec<ParserMatch<V>>> {
        Ok(self.candidates(input, dimension_prio)?
            .into_iter()
            .filter(|a| a.3)
            .map(|a| a.1)
            .collect())
    }
}

// This is maximal elements in the POSet (Partial Ordered Set) sense: all
// elements that have no "dominant" are a maximal element.
fn tag_maximal_elements<I, CMP: Fn(&I, &I) -> Option<Ordering>>(values: Vec<I>,
                                                                cmp: CMP)
                                                                -> Vec<(I, bool)> {
    let mut mask = vec!(false; values.len());
    'a: for (ix, a) in values.iter().enumerate() {
        for b in values.iter() {
            if cmp(a, b) == Some(Ordering::Less) {
                continue 'a;
            }
        }
        mask[ix] = true;
    }
    values.into_iter().zip(mask.into_iter()).collect()
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;
    use std::str::FromStr;

    use super::*;

    fn cmp(a: &&'static str, b: &&'static str) -> Option<Ordering> {
        if a == b {
            Some(Ordering::Equal)
        } else if a.starts_with(b) {
            Some(Ordering::Greater)
        } else if b.starts_with(a) {
            Some(Ordering::Less)
        } else {
            None
        }
    }

    #[test]
    fn test_adhoc_order() {
        assert_eq!(cmp(&"a", &"aa"), Some(Ordering::Less));
        assert_eq!(cmp(&"b", &"aa"), None);
        assert_eq!(cmp(&"ba", &"aa"), None);
        assert_eq!(cmp(&"aa", &"aa"), Some(Ordering::Equal));
        assert_eq!(cmp(&"aaa", &"aa"), Some(Ordering::Greater));
        assert_eq!(cmp(&"ab", &"aa"), None);
    }

    fn maximal_elements<I, CMP: Fn(&I, &I) -> Option<Ordering>>(values: Vec<I>, cmp: CMP) -> Vec<I> {
        super::tag_maximal_elements(values, cmp).into_iter().filter(|&(_, m)| m).map(|(a, _)| a).collect()
    }

    #[test]
    fn max_elements() {
        let values = vec!["ba", "baaar", "foo", "aa", "aaa", "a"];
        assert_eq!(maximal_elements(values, cmp),
                   vec!["baaar", "foo", "aaa"])
    }

    #[derive(Copy,Clone,Debug,PartialEq)]
    struct Int(usize);

    impl AttemptFrom<Int> for Int {
        fn attempt_from(v: Int) -> Option<Int> {
            Some(v)
        }
    }

    macro_rules! reg {
        ($typ:ty, $pattern:expr) => ( $crate::core::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
    }

    /*
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
    */

    fn rules() -> RuleSet<Int> {
        let rule = rule! {
            "integer (numeric)",
            (reg!(Int, r#"(\d{1,18})"#)),
            |text_match| Ok(Int(text_match.group(0).parse::<usize>()?))
        };
        let rule_thousand = rule! {
            "integer (thousand)",
            (reg!(Int, "thousands?")),
            |_| Ok(Int(1000))
        };
        let rule_compo = rule! {
            "number thousands",
            (   dim!(Int, vec!(Box::new(|a:&Int| a.0 > 1 && a.0 < 99))),
                dim!(Int, vec!(Box::new(|a:&Int| a.0 == 1000)))),
            |a,_| Ok(Int(a.value().0*1000))
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
        assert_eq!(vec![Int(12), Int(1000), Int(12000)], values);
    }

    #[test]
    fn test_integer_numeric_infix_rule() {
        let rule_int =
            rule! { "int", (reg!(Int, "\\d+")), |a| Ok(Int(usize::from_str(&*a.group(0))?)) };
        let rule_add = rule! {
            "add",
            (dim!(Int), reg!(Int, "\\+"), dim!(Int)),
            |a,_,b| Ok(Int(a.value().0+b.value().0))
        };
        let rule_mul = rule! {
            "mul",
            (dim!(Int), reg!(Int, "\\*"), dim!(Int)),
            |a,_,b| Ok(Int(a.value().0*b.value().0))
        };
        let rule_set = RuleSet(vec![rule_int, rule_add, rule_mul]);
        let results = rule_set.apply_all("foo: 12 + 42, 12* 42").unwrap();
        let values: Vec<_> = results.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![Int(12), Int(42), Int(12), Int(42), Int(54), Int(504)], values);
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
