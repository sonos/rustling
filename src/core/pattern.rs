use core::*;
use smallvec::SmallVec;

pub type Range = (usize, usize);

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
pub struct Text<'a>(pub SmallVec<[&'a str;4]>, pub Range, pub &'static str);

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
    fn predicate(&self, stash: &Stash<StashValue>, sentence: &'a str) -> Vec<M>;
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
    fn predicate(&self, _stash: &Stash<StashValue>, sentence: &'a str) -> Vec<Text<'a>> {
        self.0
            .captures_iter(&sentence)
            .map(|cap| {
                let full = cap.get(0).unwrap();
                let full_range = (full.start(), full.end());
                Text(cap.iter().map(|m| m.unwrap().as_str()).collect(),
                     full_range,
                     self.1)
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

impl<'a, StashValue, V:'a> Pattern<'a, ParsedNode<V>, StashValue> for FilterNodePattern<V>
    where StashValue: Clone,
          V: AttemptFrom<StashValue> + Clone
{
    fn predicate(&self, stash: &Stash<StashValue>, _sentence: &'a str) -> Vec<ParsedNode<V>> {
        stash.iter()
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
            .collect()
    }
}
