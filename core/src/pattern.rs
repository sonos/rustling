
use std::rc;

use smallvec::SmallVec;

use errors::*;
use {AttemptFrom, Sym, Node, ParsedNode, SendSyncPhantomData, Stash, StashIndexable, InnerStashIndexable, NodePayload};
use helpers::BoundariesChecker;
use range::Range;
use regex::Captures;

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

pub type PredicateMatches<M> = Vec<M>;

pub trait Pattern<StashValue: NodePayload+StashIndexable>: Send + Sync {
    type M: Match<NV=StashValue::Payload>;
    fn find_all<'s>(&self, stash: &Stash<'s, StashValue>)
                 -> CoreResult<PredicateMatches<Self::M>>;
    fn nearest_neighbors_from<'s>(&self, stash: &Stash<'s, StashValue>, position: usize)
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
    fn find_all<'s>(&self, stash: &Stash<'s, StashValue>)
                 -> CoreResult<PredicateMatches<Self::M>> {
        let mut results = PredicateMatches::new();
        for cap in self.pattern.captures_iter(stash.sentence) {
            match self.format_match(&cap, stash.sentence)? {
                Some(text) => results.push(text),
                None => continue,
            }
        }
        Ok(results)
    }

    fn nearest_neighbors_from<'s>(&self,
                 stash: &Stash<'s, StashValue>, position: usize)
                 -> CoreResult<PredicateMatches<Self::M>> {
        match self.pattern.captures(&stash.sentence[position..]) {
            Some(cap) => Ok(self.format_match(&cap, stash.sentence)?
                .map(|m| vec![m])
                .unwrap_or(vec![])), 
            None => Ok(vec![]),
        }
    }
}

impl<StashValue: NodePayload+StashIndexable> TextPattern<StashValue> {
    fn format_match<'t>(&self, capture: &Captures<'t>, full_sentence: &str) -> CoreResult<Option<Text<StashValue>>> {
        let full = capture.get(0)
            .ok_or_else(|| {
                            format!("No capture for regexp {} in rule {:?} for sentence: {}",
                                    self.pattern,
                                    self.pattern_sym,
                                    full_sentence)
                        })?;
        let full_range = Range(full.start(), full.end());
        if !self.boundaries_checker.check(full_sentence, full_range) {
            return Ok(None);
        }

        let mut groups = SmallVec::new();
        
        for (ix, group) in capture.iter().enumerate() {
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

        Ok(Some(Text {
            groups: groups,
            byte_range: full_range,
            pattern_sym: self.pattern_sym,
            _phantom: SendSyncPhantomData::new()
        }))
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
    fn find_all<'s>(&self,
                 stash: &Stash<'s, StashValue>)
                 -> CoreResult<PredicateMatches<Text<StashValue>>> {
        let mut results = PredicateMatches::new();
        for cap in self.pattern.captures_iter(stash.sentence) {
            match self.format_match(&cap, stash.sentence)? {
                Some(text) => results.push(text),
                None => continue,
            }
        }
        Ok(results)
    }

    fn nearest_neighbors_from<'s>(&self,
                 stash: &Stash<'s, StashValue>, position: usize)
                 -> CoreResult<PredicateMatches<Text<StashValue>>> {
        match self.pattern.captures(&stash.sentence[position..]) {
            Some(cap) => Ok(self.format_match(&cap, stash.sentence)?
                .map(|m| vec![m])
                .unwrap_or(vec![])), 
            None => Ok(vec![]),
        }
    }
}

impl<StashValue: NodePayload+StashIndexable> TextNegLHPattern<StashValue> {
    fn format_match<'t>(&self, capture: &Captures<'t>, full_sentence: &str) -> CoreResult<Option<Text<StashValue>>> {
        let full = capture.get(0)
            .ok_or_else(|| {
                            format!("No capture for regexp {} in rule {:?} for sentence: {}",
                                    self.pattern,
                                    self.pattern_sym,
                                    full_sentence)
                        })?;
        let full_range = Range(full.start(), full.end());
        if !self.boundaries_checker.check(full_sentence, full_range) {
            return Ok(None);
        }
        if let Some(mat) = self.neg_look_ahead.find(&full_sentence[full.end()..]) {
            if mat.start() == 0 {
                return Ok(None);
            }
        }
        let mut groups = SmallVec::new();
        for (ix, group) in capture.iter().enumerate() {
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
        Ok(Some(Text {
                         groups: groups,
                         byte_range: full_range,
                         pattern_sym: self.pattern_sym,
                         _phantom: SendSyncPhantomData::new(),
                     }))
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
    fn find_all<'s>(&self, stash: &Stash<'s, StashValue>)
                 -> CoreResult<PredicateMatches<ParsedNode<V>>> {
        Ok(stash.find_all(|v|{
            self.predicates.iter().all(|predicate| (predicate)(&v))
        }))
    }

    fn nearest_neighbors_from<'s>(&self, stash: &Stash<'s, StashValue>, position: usize)
                 -> CoreResult<PredicateMatches<ParsedNode<V>>> {
        Ok(stash.nearest_neighbors_from(position, |v|{
            self.predicates.iter().all(|predicate| (predicate)(&v))
        }))
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
        let checker = BoundariesChecker::Detailed;
        let pat: TextPattern<usize> = TextPattern::new(::regex::Regex::new("a+").unwrap(), Sym(0), checker);
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.find_all(&Stash::new("aaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.find_all(&Stash::new("aaa bbb")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(4, 7)), Range(4, 7), Sym(0))],
                   pat.find_all(&Stash::new("bbb aaa")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.find_all(&Stash::new("baaa")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.find_all(&Stash::new("aaab")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.find_all(&Stash::new("aaaé")).unwrap());
        assert_eq!(Vec::<Text<usize>>::new(), pat.find_all(&Stash::new("éaaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(1, 4)), Range(1, 4), Sym(0))],
                   pat.find_all(&Stash::new("1aaa")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.find_all(&Stash::new("aaa1")).unwrap());
        assert_eq!(vec![Text::new(svec4!(Range(0, 3)), Range(0, 3), Sym(0))],
                   pat.find_all(&Stash::new("aaa-toto")).unwrap());
    }
}
