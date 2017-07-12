use std::cmp::{PartialOrd, Ordering};

/// Represent a semi-inclusive range of position, in bytes, in the matched
/// sentence.
#[derive(PartialEq,Clone,Debug,Copy,Hash,Eq)]
pub struct Range(pub usize, pub usize);

impl Range {
    pub fn intersects(&self, other: &Self) -> bool {
        self.partial_cmp(other).is_none() && (self.1 >= other.0 && other.1 >= self.0)
    }

    pub fn char_range(&self, string: &str) -> Range {
        Range(convert_char_index(string, self.0), convert_char_index(string, self.1))
    }

    pub fn byte_range(&self, string: &str) -> Range {
        Range(convert_byte_index(string, self.0), convert_byte_index(string, self.1))
    }

    pub fn len(&self) -> usize {
        self.1 - self.0
    }
    
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.0 >= other.1 || other.0 >= self.1
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

pub fn convert_char_index(string: &str, byte_index: usize) -> usize {
    if string.is_empty() {
        return 0;
    }
    let mut acc = 0;
    let mut last_char_index = 0;
    for (char_index, char) in string.chars().enumerate() {
        if byte_index <= acc {
            return char_index;
        }
        acc += char.len_utf8();
        last_char_index = char_index;
    }
    last_char_index + 1
}

pub fn convert_byte_index(string: &str, char_index: usize) -> usize {
    let mut result = 0;
    for (current_char_index, char) in string.chars().enumerate() {
        if current_char_index == char_index {
            return result;
        }
        result += char.len_utf8()
    }
    result
}