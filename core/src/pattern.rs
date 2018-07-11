
use std::rc;

use smallvec::SmallVec;

use CoreResult;
use {AttemptFrom, Sym, Node, ParsedNode, SendSyncPhantomData, Stash, StashIndexable, InnerStashIndexable, NodePayload, ParsingStatus};
use helpers::BoundariesChecker;
use range::Range;
use std::slice::Iter;
use std::vec::IntoIter;

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
    pub byte_range: Range,
    pattern_sym: Sym,
     _phantom: SendSyncPhantomData<V>,
}

impl<V: NodePayload> Text<V> {
    pub fn new(groups: SmallVec<[Range; 4]>, byte_range: Range, pattern_sym: Sym) -> Text<V> {
        Text {
            groups: groups,
            byte_range: byte_range,
            pattern_sym: pattern_sym,
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<V: NodePayload> Match for Text<V> {
    type NV = V::Payload;
    fn byte_range(&self) -> Range {
        self.byte_range
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

pub struct PredicateMatches<M> {
    pub matches: Vec<M>,
    pub status: ParsingStatus,
}

impl<M> PredicateMatches<M> {
    pub fn with_status(status: ParsingStatus) -> PredicateMatches<M> {
        PredicateMatches {
            matches: vec![],
            status,
        }
    }

    pub fn continue_with(matches: Vec<M>) -> PredicateMatches<M> {
        PredicateMatches {
            matches: matches,
            status: ParsingStatus::Continue,
        }
    }

    pub fn exit_if_empty(self) -> PredicateMatches<M> {
        if self.matches.len() == 0 {
            PredicateMatches::with_status(ParsingStatus::Exit)
        } else {
            self
        }
    }

    pub fn push(&mut self, match_: M) {
        self.matches.push(match_)
    }

    pub fn is_empty(&self) -> bool {
        self.matches.is_empty()
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn iter(&self) -> Iter<M> {
        self.matches.iter()
    }

    pub fn into_iter(self) -> IntoIter<M> {
        self.matches.into_iter()
    }
}

pub trait Pattern<StashValue: NodePayload+StashIndexable>: Send + Sync {
    type M: Match<NV=StashValue::Payload>;
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 sentence: &str)
                 -> CoreResult<PredicateMatches<Self::M>>;
}

pub trait TerminalPattern<StashValue: NodePayload+StashIndexable>: Pattern<StashValue, M=Text<StashValue>> { }

pub struct TextPattern<StashValue: NodePayload+StashIndexable> {
    pattern: ::regex::Regex, 
    pattern_sym: Sym, 
    boundaries_checker: BoundariesChecker, 
    _phantom: SendSyncPhantomData<StashValue>,
}

impl<StashValue: NodePayload+StashIndexable> TextPattern<StashValue> {
    pub fn new(regex: ::regex::Regex, sym: Sym, boundaries_checker: BoundariesChecker) -> TextPattern<StashValue> {
        TextPattern {
            pattern: regex, 
            pattern_sym: sym, 
            boundaries_checker, 
            _phantom: SendSyncPhantomData::new()
        }
    }
}

impl<StashValue: NodePayload+StashIndexable> Pattern<StashValue> for TextPattern<StashValue> {
    type M = Text<StashValue>;
    fn predicate(&self,
                 _stash: &Stash<StashValue>,
                 sentence: &str)
                 -> CoreResult<PredicateMatches<Self::M>> {
        let mut results = PredicateMatches::with_status(ParsingStatus::Continue);
        for cap in self.pattern.captures_iter(&sentence) {
            let full = cap.get(0)
                .ok_or_else(|| {
                                format_err!("No capture for regexp {} in rule {:?} for sentence: {}",
                                        self.pattern,
                                        self.pattern_sym,
                                        sentence)
                            })?;
            let full_range = Range(full.start(), full.end());
            if !self.boundaries_checker.check(sentence, full_range) {
                continue;
            }
            let mut groups = SmallVec::new();
            for (ix, group) in cap.iter().enumerate() {
                let group = group.ok_or_else(|| {
                            format_err!("No capture for regexp {} in rule {:?}, group number {} in \
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
                             byte_range: full_range,
                             pattern_sym: self.pattern_sym,
                             _phantom: SendSyncPhantomData::new()
                         })
        }

        Ok(results.exit_if_empty())
    }
}

impl<StashValue: NodePayload+StashIndexable> TerminalPattern<StashValue> for TextPattern<StashValue> {}

pub struct TextNegLHPattern<StashValue: NodePayload+StashIndexable> {
    pattern: ::regex::Regex,
    neg_look_ahead: ::regex::Regex,
    boundaries_checker: BoundariesChecker,
    pattern_sym: Sym,
    _phantom: SendSyncPhantomData<StashValue>,
}

impl<StashValue: NodePayload+StashIndexable> TextNegLHPattern<StashValue> {
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

impl<StashValue: NodePayload+StashIndexable> Pattern<StashValue> for TextNegLHPattern<StashValue> {
    type M = Text<StashValue>;
    fn predicate(&self,
                 _stash: &Stash<StashValue>,
                 sentence: &str)
                 -> CoreResult<PredicateMatches<Text<StashValue>>> {
        let mut results = PredicateMatches::with_status(ParsingStatus::Continue);
        for cap in self.pattern.captures_iter(&sentence) {
            let full = cap.get(0)
                .ok_or_else(|| {
                                format_err!("No capture for regexp {} in rule {:?} for sentence: {}",
                                        self.pattern,
                                        self.pattern_sym,
                                        sentence)
                            })?;
            let full_range = Range(full.start(), full.end());
            if !self.boundaries_checker.check(sentence, full_range) {
                continue;
            }
            if let Some(mat) = self.neg_look_ahead.find(&sentence[full.end()..]) {
                if mat.start() == 0 {
                    continue;
                }
            }
            let mut groups = SmallVec::new();
            for (ix, group) in cap.iter().enumerate() {
                let group = group.ok_or_else(|| {
                            format_err!("No capture for regexp {} in rule {:?}, group number {} in \
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
                             byte_range: full_range,
                             pattern_sym: self.pattern_sym,
                             _phantom: SendSyncPhantomData::new(),
                         })
        }

        Ok(results.exit_if_empty())
    }
}

impl<StashValue: NodePayload+StashIndexable> TerminalPattern<StashValue> for TextNegLHPattern<StashValue> {}

pub type AnyNodePattern<V> = FilterNodePattern<V>;

pub struct FilterNodePattern<V>
    where V: NodePayload + InnerStashIndexable
{
    predicates: Vec<Box<Fn(&V) -> bool + Send + Sync>>,
    _phantom: SendSyncPhantomData<V>,
}

impl<V: NodePayload+InnerStashIndexable> AnyNodePattern<V> {
    pub fn new() -> AnyNodePattern<V> {
        FilterNodePattern {
            predicates: vec![],
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<V> FilterNodePattern<V>
    where V: NodePayload + InnerStashIndexable
{
    pub fn filter(predicates: Vec<Box<Fn(&V) -> bool + Sync + Send>>) -> FilterNodePattern<V> {
        FilterNodePattern {
            predicates: predicates,
            _phantom: SendSyncPhantomData::new(),
        }
    }
}

impl<StashValue, V> Pattern<StashValue> for FilterNodePattern<V>
    where StashValue: NodePayload + StashIndexable,
          V: NodePayload<Payload=StashValue::Payload> + InnerStashIndexable<Index=StashValue::Index> + AttemptFrom<StashValue>,
{
    type M = ParsedNode<V>;
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 _sentence: &str)
                 -> CoreResult<PredicateMatches<ParsedNode<V>>> {
        Ok(PredicateMatches::continue_with(stash.filter(|v|{
            self.predicates.iter().all(|predicate| (predicate)(&v))
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! svec4 {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::<[_;4]>::new();
            $( v.push($item); )*
            v
        }
        }
    }

    #[test]
    fn test_regex_separated_string() {
        let stash = Stash::default();
        let checker = BoundariesChecker::detailed();
        let pat: TextPattern<usize> = TextPattern::new(::regex::Regex::new("a+").unwrap(), Sym(0), checker);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, "aaa").unwrap().matches);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, "aaa bbb").unwrap().matches);
        assert_eq!(vec![Text::new(svec4!(Range(4, 7)), Range(4, 7), Sym(0))],
                   pat.predicate(&stash, "bbb aaa").unwrap().matches);
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, "baaa").unwrap().matches);
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, "aaab").unwrap().matches);
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, "aaaé").unwrap().matches);
        assert_eq!(Vec::<Text<usize>>::new(), pat.predicate(&stash, "éaaa").unwrap().matches);
        assert_eq!(vec![Text::new(svec4!(Range(1, 4)), Range(1, 4), Sym(0))],
                   pat.predicate(&stash, "1aaa").unwrap().matches);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, "aaa1").unwrap().matches);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.predicate(&stash, "aaa-toto").unwrap().matches);
    }
}
