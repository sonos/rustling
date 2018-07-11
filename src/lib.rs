#[macro_use]
extern crate failure;
extern crate fnv;
extern crate rustling_core;
extern crate rustling_ml;
#[macro_use]
extern crate serde_derive;

use std::collections::HashSet;

pub use rustling_core::regex;
pub use rustling_core::{AttemptFrom, AttemptInto, Sym, Node, ParsedNode, Range, RuleSet,
                        RuleSetBuilder, NodePayload, BoundariesChecker, StashIndexable, InnerStashIndexable};
pub use rustling_core::{RuleError, RuleResult};
pub use rustling_ml::{ClassId, Classifier, ClassifierId, Feature, Input, Model};
pub use train::{Check, Example};

#[macro_use]
pub mod macros;
pub mod train;

pub mod core {
    pub use rustling_core::pattern::{AnyNodePattern, FilterNodePattern, TextNegLHPattern,
                                     TextPattern};
    pub use rustling_core::rule::{Rule1, Rule2, Rule3, Rule4, Rule5, Rule6};
}

pub type RustlingResult<T> = Result<T, ::failure::Error>;

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
    /// Parsing tree height
    pub parsing_tree_height: usize,
    /// Number of nodes in the parsing tree
    pub parsing_tree_num_nodes: usize,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingAnalysis<'a> {
    /// Coverage of rules used during the analysis
    pub rules_coverage: f32,
    /// Coverage of text pattern used during the analysis
    pub text_pattern_coverage: f32,
    /// Coverage of example with only one output
    pub examples_coverage: f32,
    /// Rules' names which were not used during the analysis
    pub unused_rules: Vec<&'a str>,
    /// Text patterns's names which were not used during the analysis
    pub unused_text_pattern: Vec<&'a str>,
    /// IFailed examples with the position of the example and the number of output found. An example is a success if and only if one output is found during the parsing
    pub failed_examples: Vec<(usize, usize)>,
}

#[derive(Debug, Clone)]
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
                    parsing_tree_height: p.root_node.height(),
                    parsing_tree_num_nodes: p.root_node.num_nodes(),
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
            .filter_map(|c| {
                if c.tagged {
                    Some(c.match_)
                } else {
                    None
                }
            })
            .collect())
    }

    pub fn analyse<Tagger: MaxElementTagger<V>>(&self, examples: Vec<&str>, tagger: &Tagger) -> RustlingResult<ParsingAnalysis> {
        let all_syms = self.rules.all_syms().into_iter().collect::<HashSet<_>>();
        let rules_syms = self.rules.rules_syms().into_iter().collect::<HashSet<_>>();
        let text_pattern_syms: HashSet<_> = all_syms.difference(&rules_syms).map(|s| *s).collect();

        let mut used_syms = HashSet::new();
        let mut failed_examples = vec![];

        for (idx, example) in examples.iter().enumerate() {
            let outputs = self.candidates(example, tagger)?
                .into_iter()
                .filter(|c| c.tagged)
                .collect::<Vec<_>>();
            
            if outputs.len() != 1 {
                failed_examples.push((idx, outputs.len()));
            } else {
                for sym in outputs[0].node.root_node.all_syms().into_iter() {
                    used_syms.insert(*sym);
                }
            }
        }
        let unused_rules: Vec<_> = rules_syms.difference(&used_syms)
                    .filter_map(|s| self.resolve_sym(&s))
                    .collect();
            
        let unused_text_pattern: Vec<_> = text_pattern_syms.difference(&used_syms)
                    .filter_map(|s| self.resolve_sym(&s))
                    .collect();
            
        Ok(ParsingAnalysis {
            rules_coverage: 1.0 - (unused_rules.len() as f32 / rules_syms.len() as f32),
            text_pattern_coverage: 1.0 - (unused_text_pattern.len() as f32 / text_pattern_syms.len() as f32),
            examples_coverage: 1.0 - (failed_examples.len() as f32 / examples.len() as f32),
            unused_rules: unused_rules,
            unused_text_pattern: unused_text_pattern,
            failed_examples: failed_examples,
        })
    }

    pub fn num_rules(&self) -> usize {
        self.rules.rules_syms().into_iter().collect::<HashSet<_>>().len()
    }

    pub fn num_text_patterns(&self) -> usize {
        let all_syms = self.rules.all_syms().into_iter().collect::<HashSet<_>>();
        let rules_syms = self.rules.rules_syms().into_iter().collect::<HashSet<_>>();
        let text_pattern_syms: HashSet<_> = all_syms.difference(&rules_syms).map(|s| *s).collect();
        text_pattern_syms.len()
    }

    pub fn resolve_sym(&self, sym:&Sym) -> Option<&str> {
        self.rules.resolve_sym(sym)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use fnv::FnvHashMap;
    use super::*;
    
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
        let b = RuleSetBuilder::new(BoundariesChecker::detailed(), BoundariesChecker::separated_alphanumeric_word());
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
        let b = RuleSetBuilder::new(BoundariesChecker::detailed(), BoundariesChecker::separated_alphanumeric_word());
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

    #[derive(Debug, Hash, Clone, Eq, PartialEq,Serialize,Deserialize)]
    struct TestFeat;

    impl Feature for TestFeat {}

    struct TestFeatExtractor();

    impl FeatureExtractor<MyValue, TestFeat> for TestFeatExtractor {
        fn for_parsed_node(&self,
                           node: &ParsedNode<MyValue>)
                           -> Input<RuleId, TestFeat> {
            self.for_node(&node.root_node)
        }
        fn for_node(&self, node: &Node<usize>) -> Input<RuleId, TestFeat> {
            Input {
                classifier_id: RuleId(node.rule_sym),
                children: vec!(),
                features: vec!(),
            }
        }
    }

    struct TestMaxElementTagger;

    impl MaxElementTagger<MyValue> for TestMaxElementTagger {
        type O = MyValue;
        fn tag(&self, candidates: Vec<(ParsedNode<MyValue>, ParserMatch<MyValue>)>) -> Vec<Candidate<MyValue, MyValue>> {
            let mut candidates = candidates;
            candidates.sort_by(|a, b| {
                a.1.byte_range.len().cmp(&b.1.byte_range.len())
            });
            candidates.into_iter()
                .rev()
                .enumerate()
                .map(|(idx, c)| {
                    Candidate {
                        node: c.0,
                        match_: c.1,
                        tagged: idx == 0,
                    }
                })
                .collect()
        }
    }

    fn rules_with_enum_value() -> RuleSet<MyValue> {
        let b = RuleSetBuilder::new(BoundariesChecker::detailed(), BoundariesChecker::separated_alphanumeric_word());
        b.rule_1("int",
                 b.reg("\\d+").unwrap(),
                 |a| Ok(Int(usize::from_str(&*a.group(0))?)));
        b.rule_1("fp",
                 b.reg("\\d+\\.\\d+").unwrap(),
                 |a| Ok(F32(f32::from_str(&*a.group(0))?)));
        b.rule_3("pow",
                 dim!(F32),
                 b.reg("\\^").unwrap(),
                 dim!(Int),
                 |a, _, b| Ok(F32(a.value().0.powi(b.value().0 as i32))));
        b.build()
    }

    fn parser() -> Parser<MyValue, TestFeat, TestFeatExtractor> {
        Parser {
            rules: rules_with_enum_value(),
            model: Model { classifiers: FnvHashMap::default() },
            extractor: TestFeatExtractor(),
        }
    }

    #[test]
    fn test_with_enum_value() {
        let rule_set = rules_with_enum_value();
        let results = rule_set.apply_all("foo: 1.5^2").unwrap();
        let values: Vec<_> = results.into_iter().map(|pn| pn.value).collect();
        assert_eq!(vec![MyValue::UI(Int(1)),
                        MyValue::UI(Int(5)),
                        MyValue::UI(Int(2)),
                        MyValue::FP(F32(1.5)),
                        MyValue::FP(F32(2.25))],
                   values);
    }

    #[test]
    fn test_parsing_analysis() {
        let parser = parser();
        assert_eq!(ParsingAnalysis {
                rules_coverage: 0.6666666,
                text_pattern_coverage: 0.6666666,
                examples_coverage: 0.5,
                unused_rules: vec!["pow"],
                unused_text_pattern: vec!["\\^"],
                failed_examples: vec![
                        (0, 0), 
                        (1, 0),
                        ],
                }, 
            parser.analyse(vec!["example that should fail", "another one", "foo: 1.5", "foo: 2"], &TestMaxElementTagger).unwrap()
        );
        assert_eq!(ParsingAnalysis {
                rules_coverage: 1.0,
                text_pattern_coverage: 1.0,
                examples_coverage: 0.6666666,
                unused_rules: vec![],
                unused_text_pattern: vec![],
                failed_examples: vec![(0, 0)],}, 
            parser.analyse(vec!["example that should fail", "foo: 1.5^2", "foo: 2"], &TestMaxElementTagger).unwrap()
        );
    }
}
