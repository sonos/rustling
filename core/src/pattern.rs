use ::std::cmp::{ PartialOrd, Ordering };

use smallvec::SmallVec;

use errors::*;
use {AttemptFrom, Node, ParsedNode, Stash};

/// Represent a semi-inclusive range of position, in bytes, in the matched
/// sentence.
#[derive(PartialEq,Clone,Debug,Copy,Hash,Eq)]
pub struct Range(pub usize, pub usize);

impl Range {
    pub fn intersects(&self, other: &Self) -> bool {
        self.partial_cmp(other).is_none() && (self.1 >= other.0 && other.1 >= self.0)
    }
}

impl PartialOrd for Range {

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if self.0 <= other.0 && other.1 <= self.1 {
            Some(Ordering::Greater)
        } else if other.0 <= self.0 && self.1 <= other.1 {
            Some(Ordering::Less)
        } else {
            None
        }
    }
}

pub trait Match: Clone {
    fn range(&self) -> Range;
    fn to_node(&self) -> Node;
}

impl<V: Clone> Match for ParsedNode<V> {
    fn range(&self) -> Range {
        self.root_node.range
    }

    fn to_node(&self) -> Node {
        self.root_node.clone()
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Text {
    pub groups: SmallVec<[Range; 4]>,
    range: Range,
    pattern_name: &'static str
}

impl Text {
    pub fn new(groups: SmallVec<[Range; 4]>, range: Range, pattern_name: &'static str) -> Text {
        Text {
            groups: groups, range: range, pattern_name: pattern_name
        }
    }
}

impl Match for Text {
    fn range(&self) -> Range {
        self.range
    }

    fn to_node(&self) -> Node {
        Node {
            rule_name: self.pattern_name,
            range: self.range(),
            children: vec![],
        }
    }
}

pub type PredicateMatches<M> = Vec<M>;

pub trait Pattern<StashValue: Clone> {
    type M: Match;
    fn predicate(&self, stash: &Stash<StashValue>, sentence: &str) -> CoreResult<PredicateMatches<Self::M>>;
}


pub struct TextPattern<StashValue: Clone>(::regex::Regex,
                                          &'static str,
                                          ::std::marker::PhantomData<StashValue>);

impl<StashValue: Clone> TextPattern<StashValue> {
    pub fn new(regex: ::regex::Regex, name: &'static str) -> TextPattern<StashValue> {
        TextPattern(regex, name, ::std::marker::PhantomData)
    }
}

impl<StashValue: Clone> Pattern<StashValue> for TextPattern<StashValue> {
    type M=Text;
    fn predicate(&self, _stash: &Stash<StashValue>, sentence: &str) -> CoreResult<PredicateMatches<Self::M>> {
        self.0
            .captures_iter(&sentence)
            .map(|cap| {
                let full = cap.get(0)
                    .ok_or_else(|| {
                        format!("No capture for regexp {} in rule {} for sentence: {}",
                                self.0,
                                self.1,
                                sentence)
                    })?;
                let full_range = Range(full.start(), full.end());
                let mut groups = SmallVec::new();
                for (ix, group) in cap.iter().enumerate() {
                    let group = group.ok_or_else(|| {
                            format!("No capture for regexp {} in rule {}, group number {} in \
                                     capture: {}",
                                    self.0,
                                    self.1,
                                    ix,
                                    full.as_str())
                        })?;
                    let range = Range(group.start(), group.end());
                    groups.push(range);
                }
                Ok(Text { groups: groups, range: full_range, pattern_name: self.1 })
            })
            .collect()
    }
}

pub struct TextNegLHPattern<StashValue: Clone> {
    pattern: TextPattern<StashValue>,
    neg_look_ahead: ::regex::Regex,
    pattern_name: &'static str,
}

impl<StashValue: Clone> TextNegLHPattern<StashValue> {
    pub fn new(pattern: TextPattern<StashValue>, neg_look_ahead: ::regex::Regex, pattern_name: &'static str) -> TextNegLHPattern<StashValue> {
        TextNegLHPattern {
            pattern: pattern,
            neg_look_ahead: neg_look_ahead,
            pattern_name: pattern_name,
        }
    }
}

impl<StashValue: Clone> Pattern<StashValue> for TextNegLHPattern<StashValue> {
    type M=Text;
    fn predicate(&self, stash: &Stash<StashValue>, sentence: &str) -> CoreResult<PredicateMatches<Text>> {
        Ok(self.pattern.predicate(stash, sentence)?.into_iter().filter(|t| {
            match self.neg_look_ahead.find(&sentence[t.range().1..]) {
                None => true,
                Some(mat) => mat.start() == 0
            }
        }).map(|t| Text { pattern_name: self.pattern_name, ..t })
        .collect())
    }
}

pub type AnyNodePattern<V> = FilterNodePattern<V>;

pub struct FilterNodePattern<V>
    where V: Clone
{
    predicates: Vec<Box<Fn(&V) -> bool>>,
    _phantom: ::std::marker::PhantomData<V>,
}

impl<V: Clone> AnyNodePattern<V> {
    pub fn new() -> AnyNodePattern<V> {
        FilterNodePattern {
            predicates: vec![],
            _phantom: ::std::marker::PhantomData,
        }
    }
}

impl<V> FilterNodePattern<V>
    where V: Clone
{
    pub fn filter(predicates: Vec<Box<Fn(&V) -> bool>>) -> FilterNodePattern<V> {
        FilterNodePattern {
            predicates: predicates,
            _phantom: ::std::marker::PhantomData,
        }
    }
}

impl<StashValue, V> Pattern<StashValue> for FilterNodePattern<V>
    where StashValue: Clone,
          V: AttemptFrom<StashValue> + Clone
{
    type M=ParsedNode<V>;
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 _sentence: &str)
                 -> CoreResult<PredicateMatches<ParsedNode<V>>> {
        Ok(stash.iter()
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
            .collect())
    }
}

