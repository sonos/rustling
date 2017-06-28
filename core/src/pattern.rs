
use std::rc;

use smallvec::SmallVec;

use ::PreprocessedInput;
use errors::*;
use {AttemptFrom, Sym, Node, ParsedNode, SendSyncPhantomData, Stash, NodePayload};
use helpers::BoundariesChecker;
use range::Range;

pub trait Match: Clone {
    type NV: Clone;
    fn byte_range(&self) -> Range;
    fn to_node(&self) -> rc::Rc<Node<Self::NV>>;
}

impl<V: NodePayload> Match for ParsedNode<V> {
    type NV = V::Payload;
    fn byte_range(&self) -> Range {
        self.root_node.byte_range
    }

    fn to_node(&self) -> rc::Rc<Node<Self::NV>> {
        self.root_node.clone()
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Text<V: NodePayload> {
    pub groups: SmallVec<[Range; 4]>,
    pub preprocessed_byte_range: Range,
    pub original_byte_range: Range,
    pattern_sym: Sym,
     _phantom: SendSyncPhantomData<V>,
}

impl<V: NodePayload> Text<V> {
    pub fn new(groups: SmallVec<[Range; 4]>, preprocessed_byte_range: Range, original_byte_range: Range, pattern_sym: Sym) -> Text<V> {
        Text {
            groups,
            preprocessed_byte_range,
            original_byte_range,
            pattern_sym,
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<V: NodePayload> Match for Text<V> {
    type NV = V::Payload;
    fn byte_range(&self) -> Range {
        self.original_byte_range
    }

    fn to_node(&self) -> rc::Rc<Node<Self::NV>> {
        rc::Rc::new(Node {
                        rule_sym: self.pattern_sym,
                        byte_range: self.byte_range(),
                        payload: None,
                        children: SmallVec::new(),
                    })
    }
}

pub type PredicateMatches<M> = Vec<M>;

pub trait Pattern<StashValue: NodePayload>: Send + Sync {
    type M: Match<NV=StashValue::Payload>;
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 input: &PreprocessedInput)
                 -> CoreResult<PredicateMatches<Self::M>>;
}


pub struct TextPattern<StashValue: NodePayload> {
    pattern: ::regex::Regex, 
    pattern_sym: Sym, 
    boundaries_checker: BoundariesChecker, 
    _phantom: SendSyncPhantomData<StashValue>,
}

impl<StashValue: NodePayload> TextPattern<StashValue> {
    pub fn new(regex: ::regex::Regex, sym: Sym, boundaries_checker: BoundariesChecker) -> TextPattern<StashValue> {
        TextPattern {
            pattern: regex, 
            pattern_sym: sym, 
            boundaries_checker, 
            _phantom: SendSyncPhantomData::new()
        }
    }
}

impl<StashValue: NodePayload> Pattern<StashValue> for TextPattern<StashValue> {
    type M = Text<StashValue>;
    fn predicate(&self,
                 _stash: &Stash<StashValue>,
                 input: &PreprocessedInput)
                 -> CoreResult<PredicateMatches<Self::M>> {
        let mut results = PredicateMatches::new();
        for cap in self.pattern.captures_iter(input.preprocessed_input.as_ref()) {
            let full = cap.get(0)
                .ok_or_else(|| {
                                format!("No capture for regexp {} in rule {:?} for input: {:?}",
                                        self.pattern,
                                        self.pattern_sym,
                                        input)
                            })?;
            let preprocessed_full_range = Range(full.start(), full.end());
            let original_full_range = input.map_byte_range(&preprocessed_full_range)?;
            if !self.boundaries_checker.check(input.original_input.as_ref(), original_full_range) {
                continue;
            }
            let mut groups = SmallVec::new();
            for (ix, group) in cap.iter().enumerate() {
                let group = group.ok_or_else(|| {
                            format!("No capture for regexp {} in rule {:?}, group number {} in \
                                     capture: {}",
                                    self.pattern,
                                    self.pattern_sym,
                                    ix,
                                    full.as_str())
                        })?;
                let range = Range(group.start(), group.end());
                groups.push(range);
            }
            results.push(Text {
                             groups: groups,
                             preprocessed_byte_range: preprocessed_full_range,
                             original_byte_range: original_full_range,
                             pattern_sym: self.pattern_sym,
                             _phantom: SendSyncPhantomData::new()
                         })
        }
        Ok(results)
    }
}

pub struct TextNegLHPattern<StashValue: NodePayload> {
    pattern: ::regex::Regex,
    neg_look_ahead: ::regex::Regex,
    boundaries_checker: BoundariesChecker,
    pattern_sym: Sym,
    _phantom: SendSyncPhantomData<StashValue>,
}

impl<StashValue: NodePayload> TextNegLHPattern<StashValue> {
    pub fn new(pattern: ::regex::Regex,
               neg_look_ahead: ::regex::Regex,
               pattern_sym: Sym,
               boundaries_checker: BoundariesChecker)
               -> TextNegLHPattern<StashValue> {
        TextNegLHPattern {
            pattern,
            neg_look_ahead,
            pattern_sym,
            boundaries_checker,
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<StashValue: NodePayload> Pattern<StashValue> for TextNegLHPattern<StashValue> {
    type M = Text<StashValue>;
    fn predicate(&self,
                 _stash: &Stash<StashValue>,
                 input: &PreprocessedInput)
                 -> CoreResult<PredicateMatches<Text<StashValue>>> {
        let mut results = PredicateMatches::new();
        for cap in self.pattern.captures_iter(input.preprocessed_input.as_ref()) {
            let full = cap.get(0)
                .ok_or_else(|| {
                                format!("No capture for regexp {} in rule {:?} for input: {:?}",
                                        self.pattern,
                                        self.pattern_sym,
                                        input)
                            })?;
            let preprocessed_full_range = Range(full.start(), full.end());
            let original_full_range = input.map_byte_range(&preprocessed_full_range)?;
            if !self.boundaries_checker.check(input.original_input.as_ref(), original_full_range) {
                continue;
            }
            if let Some(mat) = self.neg_look_ahead.find(&input.preprocessed_input[full.end()..]) {
                if mat.start() == 0 {
                    continue;
                }
            }
            let mut groups = SmallVec::new();
            for (ix, group) in cap.iter().enumerate() {
                let group = group.ok_or_else(|| {
                            format!("No capture for regexp {} in rule {:?}, group number {} in \
                                     capture: {}",
                                    self.pattern,
                                    self.pattern_sym,
                                    ix,
                                    full.as_str())
                        })?;
                let range = Range(group.start(), group.end());
                groups.push(range);
            }
            results.push(Text {
                             groups: groups,
                             preprocessed_byte_range: preprocessed_full_range,
                             original_byte_range: original_full_range,
                             pattern_sym: self.pattern_sym,
                             _phantom: SendSyncPhantomData::new()
                         })
        }

        Ok(results)
    }
}

pub type AnyNodePattern<V> = FilterNodePattern<V>;

pub struct FilterNodePattern<V>
    where V: NodePayload
{
    predicates: Vec<Box<Fn(&V) -> bool + Send + Sync>>,
    _phantom: SendSyncPhantomData<V>,
}

impl<V: NodePayload> AnyNodePattern<V> {
    pub fn new() -> AnyNodePattern<V> {
        FilterNodePattern {
            predicates: vec![],
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<V> FilterNodePattern<V>
    where V: NodePayload
{
    pub fn filter(predicates: Vec<Box<Fn(&V) -> bool + Sync + Send>>) -> FilterNodePattern<V> {
        FilterNodePattern {
            predicates: predicates,
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<StashValue, V> Pattern<StashValue> for FilterNodePattern<V>
    where StashValue: NodePayload,
          V: NodePayload<Payload=StashValue::Payload> + AttemptFrom<StashValue>,
{
    type M = ParsedNode<V>;
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 _input: &PreprocessedInput)
                 -> CoreResult<PredicateMatches<ParsedNode<V>>> {
        Ok(stash
               .iter()
               .filter_map(|it| if let Some(v) = V::attempt_from(it.value.clone()) {
                               if self.predicates.iter().all(|predicate| (predicate)(&v)) {
                                   Some(ParsedNode::new(it.root_node.rule_sym,
                                                        v,
                                                        it.byte_range(),
                                                        it.root_node.payload.clone(),
                                                        it.root_node.children.clone()))
                               } else {
                                   None
                               }
                           } else {
                               None
                           })
               .collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::PreprocessedInput;

    macro_rules! svec4 {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::<[_;4]>::new();
            $( v.push($item); )*
            v
        }
        }
    }

    macro_rules! p {
        ($sentence:expr) => (PreprocessedInput::no_preprocessing($sentence))
    }

    #[test]
    fn test_regex_separated_string() {
        let stash = vec![];
        let checker = BoundariesChecker::Detailed;
        let pat: TextPattern<usize> = TextPattern::new(::regex::Regex::new("a+").unwrap(), Sym(0), checker);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, &p!("aaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, &p!("aaa bbb")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(4, 7)), Range(4, 7), Range(4, 7), Sym(0))],
                   pat.predicate(&stash, &p!("bbb aaa")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, &p!("baaa")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, &p!("aaab")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, &p!("aaaé")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, &p!("éaaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(1, 4)), Range(1, 4), Range(1, 4), Sym(0))],
                   pat.predicate(&stash, &p!("1aaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, &p!("aaa1")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, &p!("aaa-toto")).unwrap());
    }
}
