use ::std::cmp::{ PartialOrd, Ordering };

use smallvec::SmallVec;

use errors::*;
use ::*;

#[derive(PartialEq,Clone,Debug,Copy)]
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

pub trait Match<'a>: Clone {
    fn range(&self) -> Range;
    fn to_node(&self) -> Node;
}

impl<'a, V: Clone> Match<'a> for ParsedNode<V> {
    fn range(&self) -> Range {
        self.root_node.range
    }

    fn to_node(&self) -> Node {
        self.root_node.clone()
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Text<'a>(pub SmallVec<[&'a str; 4]>, pub Range, pub &'static str);

impl<'a> Match<'a> for Text<'a> {
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

pub trait Pattern<'a, M: Match<'a> + 'a, StashValue: Clone> {
    fn predicate(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Result<Vec<M>>;
}


pub struct TextPattern<StashValue: Clone>(::regex::Regex,
                                          &'static str,
                                          ::std::marker::PhantomData<StashValue>);

impl<StashValue: Clone> TextPattern<StashValue> {
    pub fn new(regex: ::regex::Regex, name: &'static str) -> TextPattern<StashValue> {
        TextPattern(regex, name, ::std::marker::PhantomData)
    }
}

impl<'a, StashValue: Clone> Pattern<'a, Text<'a>, StashValue> for TextPattern<StashValue> {
    fn predicate(&self, _stash: &Stash<StashValue>, sentence: &'a str) -> Result<Vec<Text<'a>>> {
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
                    groups.push(group.ok_or_else(|| {
                            format!("No capture for regexp {} in rule {}, group number {} in \
                                     capture: {}",
                                    self.0,
                                    self.1,
                                    ix,
                                    full.as_str())
                        })?
                        .as_str());
                }
                Ok(Text(groups, full_range, self.1))
            })
            .collect()
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

impl<'a, StashValue, V: 'a> Pattern<'a, ParsedNode<V>, StashValue> for FilterNodePattern<V>
    where StashValue: Clone,
          V: AttemptFrom<StashValue> + Clone
{
    fn predicate(&self,
                 stash: &Stash<StashValue>,
                 _sentence: &'a str)
                 -> Result<Vec<ParsedNode<V>>> {
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

