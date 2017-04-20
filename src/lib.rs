#[macro_use]
extern crate error_chain;
extern crate duckling_core as core;
extern crate duckling_ml as ml;

use ::std::cmp::{ PartialOrd, Ordering };

use errors::*;
pub mod errors {
    error_chain! {
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
pub struct Class(bool);
impl ml::ClassId for Class {}

pub trait Value: Clone+PartialEq+::std::fmt::Debug {
    fn same_dimension_as(&self, other: &Self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserMatch<V: Value> {
    pub range: core::pattern::Range,
    pub value: V,
    pub probalog: f32,
}

fn match_cmp<V,S>(a:&ParserMatch<V>, b:&ParserMatch<V>, prio:S) -> Option<Ordering> 
where
V:Value, S: Fn(&V) -> Option<usize>
{
    if a.value.same_dimension_as(&b.value) {
        if a.range == b.range {
            a.probalog.partial_cmp(&b.probalog)
        } else if a.range.intersects(&b.range) {
            b.range.0.partial_cmp(&a.range.0)
        } else {
            a.range.partial_cmp(&b.range)
        }
    } else {
        let prio_a = prio(&a.value);
        let prio_b = prio(&b.value);
        if (a.range == b.range || a.range.intersects(&b.range)) && (prio_a.is_some() && prio_b.is_some()) {
            prio_a.unwrap().partial_cmp(&prio_b.unwrap()) // checked
        } else {
            None
        }
    }
}

pub trait FeatureExtractor<V: Value, Feat: ml::Feature> {
    fn extract_features(node: &core::ParsedNode<V>) -> ml::Input<Id, Feat>;
}

pub struct Parser<'a,
V: Value,
Feat: ml::Feature,
Extractor: FeatureExtractor<V, Feat>>
{
    rules: core::RuleSet<'a, V>,
    model: ml::Model<Id, Class, Feat>,
    extractor: ::std::marker::PhantomData<Extractor>,
}

impl<'a, V, Feat, Extractor> Parser<'a, V, Feat, Extractor>
    where V: Value,
          Id: ml::ClassifierId,
          Feat: ml::Feature,
          Extractor: FeatureExtractor<V, Feat>
{
    pub fn new(rules: core::RuleSet<'a, V>,
               model: ml::Model<Id, Class, Feat>)
               -> Parser<'a, V, Feat, Extractor> {
        Parser {
            rules: rules,
            model: model,
            extractor: ::std::marker::PhantomData,
        }
    }

    pub fn parse<S: Fn(&V) -> Option<usize>>(&self, input: &'a str, dimension_prio:S) -> Result<Vec<ParserMatch<V>>> {

        let candidates = self.rules
            .apply_all(input)?
            .into_iter()
            .map(|p| {
                let features: ml::Input<Id, Feat> = Extractor::extract_features(&p);
                let probalog = self.model.classify(&features, &Class(true))?;
                Ok(ParserMatch {
                    range: p.root_node.range,
                    value: p.value,
                    probalog: probalog,
                })
            })
            .filter(|a| a.is_err() || dimension_prio(&a.as_ref().unwrap().value).is_some()) // checked
            .collect::<Result<_>>()?;
        println!("XXXX {:?}", candidates);
        Ok(maximal_elements(candidates, |a,b| match_cmp(a,b, &dimension_prio)))
    }
}

// This is maximal elements in the POSet (Partial Ordered Set) sense: all
// elements that have no "dominant" are a maximal element.
fn maximal_elements<I, CMP: Fn(&I,&I) -> Option<Ordering>>(values: Vec<I>, cmp:CMP) -> Vec<I> {
    let mut mask = vec!(false; values.len());
    'a: for (ix,a) in values.iter().enumerate() {
        for b in values.iter() {
            if cmp(a,b) == Some(Ordering::Less) {
                continue 'a;
            }
        }
        mask[ix] = true;
    }
    values.into_iter().zip(mask.into_iter()).filter(|&(_,m)| m).map(|(a,_)| a).collect()
}


#[cfg(test)]
mod tests {
    use ::std::cmp::Ordering;

        fn cmp(a:&&'static str, b: &&'static str) -> Option<Ordering> {
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
        assert_eq!(cmp(&"a",&"aa"), Some(Ordering::Less));
        assert_eq!(cmp(&"b",&"aa"), None);
        assert_eq!(cmp(&"ba",&"aa"), None);
        assert_eq!(cmp(&"aa",&"aa"), Some(Ordering::Equal));
        assert_eq!(cmp(&"aaa",&"aa"), Some(Ordering::Greater));
        assert_eq!(cmp(&"ab",&"aa"), None);
    }

    #[test]
    fn max_elements() {
        let values = vec!["ba", "baaar", "foo", "aa", "aaa", "a"];
        assert_eq!(super::maximal_elements(values, cmp), vec!["baaar", "foo", "aaa"])
    }
}
