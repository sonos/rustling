use range::Range;

#[derive(Copy,Clone, Debug, PartialEq)]
pub enum BoundariesClass {
    AlphanumericWord { separated: bool },
    AlphabeticWord { separated: bool },
    Detailed { separated: bool },
    Any,
}

impl BoundariesClass {
    pub fn apply(&self, sentence: &str, range: &Range) -> bool {
        match self {
            &BoundariesClass::AlphanumericWord { separated } => {
                let option = if separated { 
                    ValidBoundariesOption::OnCharClassChange 
                } else {
                    ValidBoundariesOption::OnSameCharClass
                };
                left_valid_boundaries(sentence, range, &option, &alphanumeric_class)
                && right_valid_boundaries(sentence, range, &option, &alphanumeric_class)
            },
            &BoundariesClass::AlphabeticWord { separated } => {
                let option = if separated { 
                    ValidBoundariesOption::OnCharClassChange 
                } else {
                    ValidBoundariesOption::OnSameCharClass
                };
                left_valid_boundaries(sentence, range, &option, &alphabetic_class)
                && right_valid_boundaries(sentence, range, &option, &alphabetic_class)
            },
            &BoundariesClass::Detailed { separated } => {
                let option = if separated { 
                    ValidBoundariesOption::OnCharClassChange 
                } else {
                    ValidBoundariesOption::OnSameCharClass
                };
                left_valid_boundaries(sentence, range, &option, &detailed_class)
                && right_valid_boundaries(sentence, range, &option, &detailed_class)            
            },
            &BoundariesClass::Any => {
                true
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoundariesChecker(Vec<BoundariesClass>);

impl BoundariesChecker {
    pub fn check(&self, sentence: &str, range: Range) -> bool {
        self.0.iter().any(|c| c.apply(sentence, &range))
    }
}

enum ValidBoundariesOption {
    OnCharClassChange,
    OnSameCharClass,
}

fn alphabetic_class(c: char) -> char {
    if c.is_alphabetic() {
        'A'
    } else {
        'O'
    }
}

fn alphanumeric_class(c: char) -> char {
    if c.is_alphanumeric() { 'A' } else { c }
}

fn detailed_class(c: char) -> char {
    if c.is_uppercase() {
        'u'
    } else if c.is_lowercase() {
        'l'
    } else if c.is_digit(10) {
        'd'
    } else {
        c
    }
}

fn right_valid_boundaries<CharClass>(sentence: &str, range: &Range, option: &ValidBoundariesOption, char_class: &CharClass) -> bool
    where CharClass: Fn(char) -> char
{
    let last_mine = sentence[range.0..range.1]
        .chars()
        .next_back()
        .map(char_class); //Some(c)
    let first_after = sentence[range.1..].chars().next().map(char_class); // Option(c)

    match option {
        &ValidBoundariesOption::OnCharClassChange => {
            last_mine != first_after
        },
        &ValidBoundariesOption::OnSameCharClass => {
            first_after == None || last_mine == first_after
        }
    }
}

fn left_valid_boundaries<CharClass>(sentence: &str, range: &Range, option: &ValidBoundariesOption, char_class: &CharClass) -> bool
    where CharClass: Fn(char) -> char
{
    let first_mine = sentence[range.0..range.1]
        .chars()
        .next()
        .map(char_class); // Some(c)
    let last_before = sentence[..range.0].chars().next_back().map(char_class); // Option(c)

    match option {
        &ValidBoundariesOption::OnCharClassChange => {
            first_mine != last_before
        },
        &ValidBoundariesOption::OnSameCharClass => {
            first_mine == None || first_mine == last_before
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_boundaries_alphanumeric() {
        let checker = BoundariesChecker::SperatedAlphanumericWord;
        assert_eq!(true, checker.check("abc def ret", Range(4, 7))); // "def"
        assert_eq!(false, checker.check("abc def ret", Range(2, 8))); // "c def r"
        assert_eq!(false, checker.check("abc def123 ret", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("def123 ret", Range(0, 6))); // "def123"
        assert_eq!(false, checker.check("def123 ret", Range(0, 3))); // "def"
        assert_eq!(true, checker.check("ret def", Range(4, 7))); // "def"
        assert_eq!(false, checker.check("ret 123def", Range(7, 10))); // "def"
        assert_eq!(false, checker.check("aéc def ret", Range(3, 9))); // "c def r"
        assert_eq!(false, checker.check("aec def rét", Range(2, 8))); // "c def r"
        assert_eq!(false, checker.check("aec déf ret", Range(2, 9))); // "c déf r"
        assert_eq!(false, checker.check("aeç def ret", Range(2, 9))); // "ç def r"
        assert_eq!(true, checker.check("aeç def ret", Range(4, 8))); // " def "
    }

    #[test]
    fn test_valid_boundaries_composed_word_or_detailed() {
        let checker = BoundariesChecker::ComposedWordOrDetailed;
        assert_eq!(true, checker.check("abc def ret", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("abc def ret", Range(2, 8))); // "c def r"
        assert_eq!(true, checker.check("abc def123 ret", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("def123 ret", Range(0, 6))); // "def123"
        assert_eq!(true, checker.check("def123 ret", Range(0, 3))); // "def"
        assert_eq!(true, checker.check("ret def", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("ret 123def", Range(7, 10))); // "def"
        assert_eq!(true, checker.check("aéc def ret", Range(3, 9))); // "c def r"
        assert_eq!(true, checker.check("aec def rét", Range(2, 8))); // "c def r"
        assert_eq!(true, checker.check("aec déf ret", Range(2, 9))); // "c déf r"
        assert_eq!(true, checker.check("aeç def ret", Range(2, 9))); // "ç def r"
        assert_eq!(true, checker.check("aeç def ret", Range(4, 8))); // " def "
    }

    #[test]
    fn test_valid_boundaries_detailed() {
        let checker = BoundariesChecker::Detailed;
        assert_eq!(true, checker.check("abc def ret", Range(4, 7))); // "def"
        assert_eq!(false, checker.check("abc def ret", Range(2, 8))); // "c def r"
        assert_eq!(true, checker.check("abc def123 ret", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("def123 ret", Range(0, 6))); // "def123"
        assert_eq!(true, checker.check("def123 ret", Range(0, 3))); // "def"
        assert_eq!(true, checker.check("ret def", Range(4, 7))); // "def"
        assert_eq!(true, checker.check("ret 123def", Range(7, 10))); // "def"
        assert_eq!(false, checker.check("aéc def ret", Range(3, 9))); // "c def r"
        assert_eq!(false, checker.check("aec def rét", Range(2, 8))); // "c def r"
        assert_eq!(false, checker.check("aec déf ret", Range(2, 9))); // "c déf r"
        assert_eq!(false, checker.check("aeç def ret", Range(2, 9))); // "ç def r"
        assert_eq!(true, checker.check("aeç def ret", Range(4, 8))); // " def "
    }
}