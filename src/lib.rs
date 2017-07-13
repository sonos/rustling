#[macro_use]
extern crate error_chain;
extern crate fnv;
extern crate rustling_core;
extern crate rustling_ml;
#[macro_use]
extern crate serde_derive;

pub use rustling_core::regex;
pub use rustling_core::{AttemptFrom, AttemptInto, Sym, Node, ParsedNode, Range, RuleSet,
                        RuleSetBuilder, NodePayload, BoundariesChecker, StashIndexable, InnerStashIndexable};
pub use rustling_core::{RuleError, RuleErrorKind, RuleResult};
pub use rustling_ml::{ClassId, Classifier, ClassifierId, Feature, Input, Model};
pub use train::{Check, Example};
pub use errors::*;

#[macro_use]
pub mod macros;
pub mod train;

pub mod core {
    pub use rustling_core::pattern::{AnyNodePattern, FilterNodePattern, TextNegLHPattern,
                                     TextPattern};
    pub use rustling_core::rule::{Rule1, Rule2, Rule3, Rule4, Rule5, Rule6};
}

pub mod errors {
    error_chain! {
        types {
            RustlingError, RustlingErrorKind, RustlingResultExt, RustlingResult;
        }
        links {
            Core(::rustling_core::errors::CoreError, ::rustling_core::errors::CoreErrorKind);
            ML(::rustling_ml::errors::MLError, ::rustling_ml::errors::MLErrorKind);
        }
        foreign_links {
            Regex(::regex::Error);
        }
        errors {
            ProductionRuleError(t: String)
        }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq,Serialize,Deserialize)]
pub struct RuleId(pub Sym);
impl ClassifierId for RuleId {}

#[derive(Debug, Hash, Clone, Eq, PartialEq,Serialize,Deserialize)]
pub struct Truth(pub bool);
impl ClassId for Truth {}

pub trait Value: NodePayload {
    type Kind: PartialEq;
    fn kind(&self) -> Self::Kind;
    fn latent(&self) -> bool;
}

/// Match holder for the Parser.
#[derive(Debug, Clone, PartialEq)]
pub struct ParserMatch<V> {
    /// Range in bytes of matched area
    pub byte_range: Range,
    /// Range in char of matched area
    pub char_range: Range,
    /// Actual value built from the text.
    pub value: V,
    /// Logarithmic probability of the match after machine-learned model
    /// evaluation.
    pub probalog: f32,
    pub latent: bool,
}

pub trait MaxElementTagger<V: Value> {
    type O;
    fn tag(&self, candidates: Vec<(ParsedNode<V>, ParserMatch<V>)>) -> Vec<Candidate<V, Self::O>>;
}

pub trait FeatureExtractor<V: Value, Feat: Feature> {
    fn for_parsed_node(&self, node: &ParsedNode<V>) -> Input<RuleId, Feat>;
    fn for_node(&self, node: &Node<V::Payload>) -> Input<RuleId, Feat>;
}

#[derive(Debug)]
pub struct Candidate<V: Value, ResolvedV> {
    pub node: ParsedNode<V>,
    pub match_: ParserMatch<ResolvedV>,
    pub tagged: bool,
}

pub struct Parser<V, Feat, Extractor>  
    where V: Value + StashIndexable,
          Feat: Feature,
          Extractor: FeatureExtractor<V, Feat> {
    rules: RuleSet<V>,
    model: Model<RuleId, Truth, Feat>,
    extractor: Extractor,
}

impl<V, Feat, Extractor> Parser<V, Feat, Extractor>
    where V: Value + ::std::fmt::Debug + StashIndexable,
          RuleId: ClassifierId,
          Feat: Feature,
          Extractor: FeatureExtractor<V, Feat>
    {
    pub fn new(rules: RuleSet<V>,
               model: Model<RuleId, Truth, Feat>,
               extractor: Extractor)
               -> Parser<V, Feat, Extractor> {
        Parser { rules, model, extractor }
    }

    fn raw_candidates(&self, input: &str) -> RustlingResult<Vec<(ParsedNode<V>, ParserMatch<V>)>> {
        self.rules
            .apply_all(input)?
            .into_iter()
            .map(|p| {
                let features: Input<RuleId, Feat> = self.extractor.for_parsed_node(&p);
                let probalog = self.model.classify(&features, &Truth(true))?;
                let pm = ParserMatch {
                    byte_range: p.root_node.byte_range,
                    char_range: p.root_node.byte_range.char_range(input),
                    value: p.value.clone().into(),
                    probalog: probalog,
                    latent: p.value.latent(),
                };
                Ok((p, pm))
            })
            .collect()
    }

    pub fn candidates<Tagger: MaxElementTagger<V>>(&self, input: &str, tagger: &Tagger) -> RustlingResult<Vec<Candidate<V, Tagger::O>>> {
        Ok(tagger.tag(self.raw_candidates(input)?))
    }

    pub fn parse<Tagger: MaxElementTagger<V>>(&self, input: &str, tagger: &Tagger) -> RustlingResult<Vec<ParserMatch<Tagger::O>>> {
        Ok(self.candidates(input, tagger)?
            .into_iter()
            .filter(|c| c.tagged)
            .map(|c| c.match_)
            .collect())
    }

    pub fn resolve_sym(&self, sym:&Sym) -> Option<&str> {
        self.rules.resolve_sym(sym)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use super::*;

    // fn cmp(a: &&str, b: &&str) -> Option<Ordering> {
    //     if a == b {
    //         Some(Ordering::Equal)
    //     } else if a.starts_with(b) {
    //         Some(Ordering::Greater)
    //     } else if b.starts_with(a) {
    //         Some(Ordering::Less)
    //     } else {
    //         None
    //     }
    // }

    // #[test]
    // fn test_adhoc_order() {
    //     assert_eq!(cmp(&"a", &"aa"), Some(Ordering::Less));
    //     assert_eq!(cmp(&"b", &"aa"), None);
    //     assert_eq!(cmp(&"ba", &"aa"), None);
    //     assert_eq!(cmp(&"aa", &"aa"), Some(Ordering::Equal));
    //     assert_eq!(cmp(&"aaa", &"aa"), Some(Ordering::Greater));
    //     assert_eq!(cmp(&"ab", &"aa"), None);
    // }

    // fn maximal_elements<I, CMP: Fn(&I, &I) -> Option<Ordering>>(values: Vec<I>,
    //                                                             cmp: CMP)
    //                                                             -> Vec<I> {
    //     super::tag_maximal_elements(values, cmp)
    //         .into_iter()
    //         .filter(|&(_, m)| m)
    //         .map(|(a, _)| a)
    //         .collect()
    // }

    // #[test]
    // fn max_elements() {
    //     let values = vec!["ba", "baaar", "foo", "aa", "aaa", "a"];
    //     assert_eq!(maximal_elements(values, cmp), vec!["baaar", "foo", "aaa"])
    // }

    #[derive(Copy,Clone,Debug,PartialEq)]
    pub struct MyPayload;

    #[derive(Copy,Clone,Debug,PartialEq,Default)]
    pub struct Int(usize);

    impl StashIndexable for Int {
        type Index = MyValueKind;
        fn index(&self) -> Self::Index {
            MyValueKind::UI
        }
    }

    #[derive(Copy,Clone,Debug,PartialEq,Default)]
    pub struct F32(f32);

    impl AttemptFrom<Int> for Int {
        fn attempt_from(v: Int) -> Option<Int> {
            Some(v)
        }
    }

    fn rules() -> RuleSet<Int> {
        let b = RuleSetBuilder::new(BoundariesChecker::Detailed);
        b.rule_1("integer (numeric)",
                 b.reg(r#"(\d{1,18})"#).unwrap(),
                 |text_match| Ok(Int(text_match.group(0).parse::<usize>()?)));
        b.rule_1("integer (thousand)",
                 b.reg("thousands?").unwrap(),
                 |_| Ok(Int(1000)));
        b.rule_2("number thousands",
                 dim!(Int, vec![Box::new(|a: &Int| a.0 > 1 && a.0 < 99)]),
                 dim!(Int, vec![Box::new(|a: &Int| a.0 == 1000)]),
                 |a, _| Ok(Int(a.value().0 * 1000)));
        b.build()
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
        let b = RuleSetBuilder::new(BoundariesChecker::Detailed);
        b.rule_1("int",
                 b.reg("\\d+").unwrap(),
                 |a| Ok(Int(usize::from_str(&*a.group(0))?)));
        b.rule_3("add",
                 dim!(Int),
                 b.reg("\\+").unwrap(),
                 dim!(Int),
                 |a, _, b| Ok(Int(a.value().0 + b.value().0)));
        b.rule_3("mul",
                 dim!(Int),
                 b.reg("\\*").unwrap(),
                 dim!(Int),
                 |a, _, b| Ok(Int(a.value().0 * b.value().0)));
        let rs = b.build();
        let results = rs.apply_all("foo: 12 + 42, 12* 42").unwrap();
        let values: Vec<_> = results.iter().map(|pn| pn.value).collect();
        assert_eq!(vec![Int(12), Int(42), Int(12), Int(42), Int(54), Int(504)],
                   values);
    }

    rustling_value! {
        #[doc="an union"]
        #[derive(Clone,PartialEq,Debug)]
        MyValue MyValueKind {
            UI(Int),
            FP(F32),
        }

        fn latent(v: &MyValue) -> bool {
            false
        }

        fn extract_payload(v: &MyValue) -> Option<usize> {
            None
        }
        
    }

    #[test]
    fn test_with_enum_value() {
        let b = RuleSetBuilder::new(BoundariesChecker::Detailed);
        b.rule_1("int",
                 b.reg("\\d+").unwrap(),
                 |a| Ok(Int(usize::from_str(&*a.group(0))?)));
        b.rule_1("fp",
                 b.reg("\\d+.\\d+").unwrap(),
                 |a| Ok(F32(f32::from_str(&*a.group(0))?)));
        b.rule_3("pow",
                 dim!(F32),
                 b.reg("\\^").unwrap(),
                 dim!(Int),
                 |a, _, b| Ok(F32(a.value().0.powi(b.value().0 as i32))));
        let rule_set = b.build();
        let results = rule_set.apply_all("foo: 1.5^2").unwrap();
        let values: Vec<_> = results.into_iter().map(|pn| pn.value).collect();
        assert_eq!(vec![MyValue::UI(Int(1)),
                        MyValue::UI(Int(5)),
                        MyValue::UI(Int(2)),
                        MyValue::FP(F32(1.5)),
                        MyValue::FP(F32(2.25))],
                   values);
    }

    // #[test]
    // fn test_filter_overlap() {
    //     let matches = vec![
    //         ParserMatch { byte_range: Range(0, 3), char_range: Range(0, 3), value: 1, probalog: 1.0, latent: true },
    //         ParserMatch { byte_range: Range(1, 4), char_range: Range(1, 4), value: 2, probalog: 1.0, latent: true },
    //     ];

    //     assert_eq!(vec![1], filter_overlap(matches.clone()).iter().map(|a| a.value).collect::<Vec<_>>());

    //     let matches = vec![
    //         ParserMatch { byte_range: Range(0, 3), char_range: Range(0, 3), value: 1, probalog: 1.0, latent: true },
    //         ParserMatch { byte_range: Range(0, 3), char_range: Range(0, 3), value: 2, probalog: 1.0, latent: true },
    //     ];

    //     assert_eq!(vec![1], filter_overlap(matches.clone()).iter().map(|a| a.value).collect::<Vec<_>>());

    //     let matches = vec![
    //         ParserMatch { byte_range: Range(3, 4), char_range: Range(3, 4), value: 2, probalog: 1.0, latent: true },
    //         ParserMatch { byte_range: Range(0, 3), char_range: Range(0, 3), value: 1, probalog: 1.0, latent: true },
    //     ];

    //     assert_eq!(vec![1, 2], filter_overlap(matches.clone()).iter().map(|a| a.value).collect::<Vec<_>>());

    //     let matches = vec![
    //         ParserMatch { byte_range: Range(3, 5), char_range: Range(3, 5), value: 1, probalog: 1.0, latent: true },
    //         ParserMatch { byte_range: Range(0, 3), char_range: Range(0, 3), value: 2, probalog: 1.0, latent: true },
    //         ParserMatch { byte_range: Range(2, 4), char_range: Range(2, 4), value: 3, probalog: 1.0, latent: true },
    //     ];

    //     assert_eq!(vec![2, 1], filter_overlap(matches.clone()).iter().map(|a| a.value).collect::<Vec<_>>());
    // }
}
