#[macro_use]
extern crate error_chain;
extern crate duckling_core as core;
extern crate duckling_ml as ml;

use ::std::cmp::{PartialOrd, Ordering};

use errors::*;

pub use core::{Node, ParsedNode, Range, RuleSet};
pub use ml::{ Feature, Model };

pub mod train;

pub mod errors {
    error_chain! {
        types {
            DucklingError, DucklingErrorKind, DucklingResultExt, DucklingResult;
        }
        links {
            Core(::core::errors::Error, ::core::errors::ErrorKind);
            ML(::ml::errors::MLError, ::ml::errors::MLErrorKind);
        }

        errors {
            ProductionRuleError(t: String)
        }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Id(pub &'static str);
impl ml::ClassifierId for Id {}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Class(pub bool);
impl ml::ClassId for Class {}

pub trait Value: Clone + PartialEq + ::std::fmt::Debug + ::std::fmt::Display {
    fn same_dimension_as(&self, other: &Self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserMatch<V: Value> {
    pub range: core::pattern::Range,
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

pub trait FeatureExtractor<V: Value, Feat: ml::Feature> {
    fn for_parsed_node(&self, node:&ParsedNode<V>) -> ml::Input<Id, Feat>;
    fn for_node(&self, node:&Node) -> ml::Input<Id, Feat>;
}

pub struct Parser<V: Value, Feat: ml::Feature, Extractor: FeatureExtractor<V, Feat>> {
    rules: core::RuleSet<V>,
    model: ml::Model<Id, Class, Feat>,
    extractor: Extractor,
}

impl<V, Feat, Extractor> Parser<V, Feat, Extractor>
    where V: Value,
          Id: ml::ClassifierId,
          Feat: ml::Feature,
          Extractor: FeatureExtractor<V, Feat>
{
    pub fn new(rules: core::RuleSet<V>,
               model: ml::Model<Id, Class, Feat>,
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
                let features: ml::Input<Id, Feat> = self.extractor.for_parsed_node(&p);
                let probalog = self.model.classify(&features, &Class(true))?;
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
    use ::std::cmp::Ordering;

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
        assert_eq!(super::maximal_elements(values, cmp),
                   vec!["baaar", "foo", "aaa"])
    }
}
