#![allow(dead_code)]

extern crate regex;
use regex::Regex;

trait Value: Clone {}
impl Value for usize {}


#[derive(Debug,PartialEq,Clone)]
struct IntegerValue { value: i64, grain: Option<u8>, group: bool, precision: Precision }
impl Value for IntegerValue {}

#[derive(Debug,PartialEq,Clone)]
struct FloatValue { value: f64, prefixed: bool, suffixed: bool, precision: Precision }
impl Value for FloatValue {}

#[derive(Debug, PartialEq,Clone)]
struct OrdinalValue { value: i64 }
impl Value for OrdinalValue {}

#[derive(Debug,PartialEq,Copy,Clone)]
enum Precision {
    Approximate,
    Exact,
}

trait Match {}

struct Text(Vec<String>);
impl Match for Text {}

struct ParsedNode<V: Value>(V);
impl<V: Value> Match for ParsedNode<V> {}

trait Pattern<M: Match> {
    fn predicate() -> M;
}

#[derive(Debug)]
pub struct TextPattern(pub Regex);

impl Pattern<Text> for TextPattern {
    fn predicate() -> Text {
        unimplemented!()
    }
}

struct IntegerNodePattern {
    min: i64,
    max: i64,
}

impl IntegerNodePattern {
    fn new(min: i64, max: i64) -> IntegerNodePattern {
        IntegerNodePattern { min: min, max: max }
    }
}

impl Pattern<ParsedNode<IntegerValue>> for IntegerNodePattern {
    fn predicate() -> ParsedNode<IntegerValue> {
        unimplemented!()
    }
}

struct DimensionPattern<V: Value> {
    _phantom: ::std::marker::PhantomData<V>
}

impl<V: Value> DimensionPattern<V> {
    fn new(_phantom: ::std::marker::PhantomData<V>) -> DimensionPattern<V> {
        DimensionPattern { _phantom: _phantom }
    }
}

impl<V: Value> Pattern<ParsedNode<V>> for DimensionPattern<V> {
    fn predicate() -> ParsedNode<V> {
        unimplemented!()
    }
}

struct DimensionPatternWithPredicates<V: Value, F> where F: Fn(V) -> bool  {
    predicates: Vec<F>,
    _phantom: ::std::marker::PhantomData<V>,
}

impl<V: Value, F> DimensionPatternWithPredicates<V, F> where F: Fn(V) -> bool {
    fn new(predicates: Vec<F>, phantom: ::std::marker::PhantomData<V>) -> DimensionPatternWithPredicates<V, F> {
        DimensionPatternWithPredicates { predicates: predicates, _phantom: phantom }
    }
}

impl<V: Value, F> Pattern<ParsedNode<V>> for DimensionPatternWithPredicates<V, F> where F: Fn(V) -> bool {
    fn predicate() -> ParsedNode<V> {
        unimplemented!()
    }
}

struct IntegerNodePatternWithPredicates<F> where F: Fn(IntegerValue) -> bool {
    min: i64,
    max: i64,
    predicates: Vec<F>
}

impl<F> IntegerNodePatternWithPredicates<F> where F: Fn(IntegerValue) -> bool {
    fn new(min: i64, max: i64, predicates: Vec<F>) -> IntegerNodePatternWithPredicates<F> {
        IntegerNodePatternWithPredicates { min: min, max: max, predicates: predicates}
    }
}

impl<F> Pattern<ParsedNode<IntegerValue>> for IntegerNodePatternWithPredicates<F>
    where F: Fn(IntegerValue) -> bool {
    fn predicate() -> ParsedNode<IntegerValue> {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub start: usize,
    pub end: usize, // excluded
}

mod try3 {
    use super::*;
    type RuleName = &'static str;

    macro_rules! R {
        ($name:ident, ($(($m:ident,$p:ident)),*)) => {
            #[allow(dead_code)]
            struct $name<V, F, $($m),*, $($p),*>
                where   V: Value,
                        $($m: Match),*,
                        $($p: Pattern<$m>),*,
                        F: Fn(($($m),*)) -> V
            {
                name: RuleName,
                pattern: ($($p),*),
                production: F,
                _phantom: ::std::marker::PhantomData<(V, $($m),*)>,
            }

            impl<V, F, $($m),*, $($p),*> $name<V,F, $($m),*, $($p),*>
                where   V: Value,
                        $($m: Match),*,
                        $($p: Pattern<$m>),*,
                        F: Fn(($($m),*)) -> V
            {
                fn new(rule_name: RuleName, pats:(($($p),*)),f:F) -> $name<V, F, $($m),*, $($p),*> {
                    $name {
                        name: rule_name,
                        pattern: pats,
                        production: f,
                        _phantom: ::std::marker::PhantomData,
                    }
                }
                #[allow(dead_code)]
                fn matches(&self) -> Vec<($($m),*)> {
                    unimplemented!()
                }
                #[allow(dead_code)]
                fn produce(&self, matches: ($($m),*)) -> ParsedNode<V> {
                    ParsedNode((self.production)(matches))
                }
            }
        }
    }

    R!(Rule1, ((A, PA)));
    R!(Rule2, ((A, PA), (B, PB)));
    R!(Rule3, ((A, PA), (B, PB), (C, PC)));

    macro_rules! rule {
        ($rule_name:expr, ($a:expr), $f:expr) => { Rule1::new($rule_name, $a, $f) };
        ($rule_name:expr, ($a:expr, $b:expr), $f:expr) => { Rule2::new($rule_name, ($a, $b), $f) };
        ($rule_name:expr, ($a:expr, $b:expr, $c:expr), $f:expr) => { Rule3::new($rule_name, ($a, $b, $c), $f) };
    }

    macro_rules! r {
        ($s:expr,$e:expr) => ( Range::new($s, $e) )
    }

    macro_rules! reg {
        ($pattern:expr) => ( TextPattern(Regex::new($pattern).unwrap()) )
    }

    macro_rules! integer_check {
        ($min:expr, $max:expr) => ( IntegerNodePattern::new($min, $max) );
        ($min:expr, $max:expr, $predicates:expr) => ( IntegerNodePatternWithPredicates::new($min, $max, $predicates) );
        () => ( DimensionPattern::new(::std::marker::PhantomData::<IntegerValue>) );
        ($predicates:expr) => ( DimensionPatternWithPredicates::new($predicates, ::std::marker::PhantomData::<IntegerValue>));
    }

    macro_rules! float_check {
        () => ( DimensionPattern::new(::std::marker::PhantomData::<FloatValue>) );
        ($predicates:expr) => ( DimensionPatternWithPredicates::new($predicates, ::std::marker::PhantomData::<FloatValue>));
    }

    macro_rules! ordinal_check {
        () => ( DimensionPattern::new(::std::marker::PhantomData::<OrdinalValue>) );
        ($predicates:expr) => ( DimensionPatternWithPredicates::new($predicates, ::std::marker::PhantomData::<OrdinalValue>));
    }

    mod tests {
        use super::*;
        use Precision::*;
        #[test]
        fn it_works() {

            let _ = rule! { 
                "ten", 
                (reg!(r#"ten"#)), 
                |_| IntegerValue { value: 10, grain: Some(1), group: false, precision: Exact } 
            };
            let _ = rule! { 
                "single", 
                (reg!(r#"single"#)), 
                |_| IntegerValue { value: 1, grain: Some(1), group: false, precision: Exact }
            };

            let _ = rule! { 
                "a pair", 
                (reg!(r#"a pair( of)?"#)),
                |_| IntegerValue { value: 2, grain: Some(1), group: false, precision: Exact }
            };

            let _ = rule! {
                "dozen",
                (reg!(r#"dozen"#)),
                |_| IntegerValue { value: 12, grain: Some(1), group: false, precision: Exact }

            };

            let _ = rule! {
                "hundred",
                (reg!(r#"hundreds?"#)),
                |_| IntegerValue { value: 100, grain: Some(2), group: false, precision: Exact }
            };

            let _ = rule! {
                "thousand",
                (reg!(r#"thousands?"#)),
                |_| IntegerValue { value: 1000, grain: Some(3), group: false, precision: Exact } 
            };

            let _ = rule! {
                "million",
                (reg!(r#"millions?"#)),
                |_| IntegerValue { value: 1000000, grain: Some(6), group: false, precision: Exact }
            };

            let _ = rule! {
                "couple",
                (reg!(r#"(a )?couple( of)?"#)),
                |_| IntegerValue { value: 2, grain: Some(1), group: false, precision: Exact }
            };

            let _ = rule! {
                "few",
                (reg!(r#"(a )?few"#)),
                |_| IntegerValue { value: 3, grain: Some(1), group: false, precision: Approximate }
            };

            let _ = rule! {
                "integer (20..90)",
                (reg!(r#"(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"#)),
                |text_match| {
                    let value = match text_match.0[1].as_ref()  {
                        "twenty"  => 20, 
                        "thirty"  => 30, 
                        "fourty"  => 40, 
                        "forty"   => 40, 
                        "fifty"   => 50, 
                        "sixty"   => 60,
                        "seventy" => 70, 
                        "eighty"  => 80, 
                        "ninety"  => 90,
                        _ => panic!("Unknow match"),
                    };
                    IntegerValue { value: value, grain: Some(1), group: false, precision: Exact }
                }
            };

            let _ = rule! {
                "integer 21..99",
                (
                    integer_check!(10, 90, vec![|integer: IntegerValue| integer.value % 10 == 0]), 
                    integer_check!(1, 9 )
                ),
                |(a, b)| IntegerValue { value: a.0.value + b.0.value, grain: None, group: false, precision: Exact }
            };

            let _ = rule! { 
                "integer (numeric)",
                (reg!(r#"(\d{1,18})"#)),
                |text_match| {
                    let value: i64 = text_match.0[0].parse().unwrap();
                    IntegerValue { value: value, grain: None, group: false, precision: Exact }
                }
            };

            let _ = rule! {
                "integer with thousands separator ,",
                (reg!(r#"(\d{1,3}(,\d\d\d){1,5})"#)),
                |text_match| {
                    let reformatted_string = text_match.0[1].replace(",", "");
                    let value: i64 = reformatted_string.parse().unwrap();
                    IntegerValue { value: value, grain: None, group: false, precision: Exact }
                }
            };

            let _ = rule! {
                "special composition for missing hundreds like in one twenty two",
                (
                    integer_check!(1, 9),
                    integer_check!(10, 99)
                ),
                |(a, b)| {
                    let value = a.0.value * 100 + b.0.value;
                    IntegerValue { value: value, grain: Some(1), group: false, precision: Exact }
                }
            };

           let _ = rule! {
               "number dozen",
               (
                   integer_check!(1, 10),
                   integer_check!(vec![|integer: IntegerValue| integer.group])
               ),
               |(a, b)| IntegerValue { value: a.0.value * b.0.value, grain: b.0.grain, group: false, precision: Exact }
           };

           let _ = rule! {
                "number hundreds",
                (
                    integer_check!(1, 99),
                    integer_check!(100, 100)
                ),
                |(a, b)| IntegerValue { value: a.0.value * b.0.value, grain: b.0.grain, group: false, precision: Exact }
           };

           let _ = rule! {
                "number thousands",
                (
                    integer_check!(1, 999),
                    integer_check!(1000, 1000)
                ),
                |(a, b)| IntegerValue { value: a.0.value * b.0.value, grain: b.0.grain, group: false, precision: Exact }
           };

           let _ = rule! {
                "number millions",
                (
                    integer_check!(1, 99),
                    integer_check!(1000000, 1000000)
                ),
                |(a, b)| IntegerValue { value: a.0.value * b.0.value, grain: b.0.grain, group: false, precision: Exact }
           };

           let _ = rule! {
                "decimal number",
                (reg!(r#"(\d*\.\d+)"#)),
                |text_match| {
                    let value: f64 = text_match.0[0].parse().unwrap();
                    FloatValue { value: value, prefixed: false, suffixed: false, precision: Exact }
                }
           };

           // FIXME This rule should work with float and integer
           let _ = rule! {
                "number dot number",
                (
                    float_check!(vec![|float: FloatValue| !float.prefixed ]),
                    reg!(r#"dot|point"#),
                    float_check!(vec![|float: FloatValue| !float.suffixed ])
                ),
                |(a, _, b)| FloatValue { value: b.0.value * 0.1 + a.0.value, prefixed: false, suffixed: false, precision: Exact }
           };

           let _ = rule! {
                "decimal with thousands separator",
                (reg!(r#"(\d+(,\d\d\d)+\.\d+)"#)),
                |text_match| {
                    let reformatted_string = text_match.0[1].replace(",", "");
                    let value: f64 = reformatted_string.parse().unwrap();
                    FloatValue { value: value, prefixed: false, suffixed: false, precision: Exact }
                }
           };
           // numbers prefix with -, negative or minus
           // numbers suffixes (K, M, G)

           let _ = rule! {
                "ordinals (first..31st)",
                (reg!(r#"(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth|seventeenth|eighteenth|nineteenth|twentieth|twenty-first|twenty-second|twenty-third|twenty-fourth|twenty-fifth|twenty-sixth|twenty-seventh|twenty-eighth|twenty-ninth|thirtieth|thirty-first)"#)),
                |text_match| {
                    let value = match text_match.0[1].as_ref() {
                        "first" => 1, 
                        "second" => 2, 
                        "third" => 3, 
                        "fourth" => 4, 
                        "fifth" => 5,
                        "sixth" => 6, 
                        "seventh" => 7, 
                        "eighth" => 8, 
                        "ninth" => 9, 
                        "tenth" => 10, 
                        "eleventh" => 11,
                        "twelfth" => 12, 
                        "thirteenth" => 13, 
                        "fourteenth" => 14, 
                        "fifteenth" => 15, 
                        "sixteenth" => 16,
                        "seventeenth" => 17, 
                        "eighteenth" => 18, 
                        "nineteenth" => 19, 
                        "twentieth" => 20, 
                        "twenty-first" => 21,
                        "twenty-second" => 22, 
                        "twenty-third" => 23, 
                        "twenty-fourth" => 24, 
                        "twenty-fifth" => 25,
                        "twenty-sixth" => 26, 
                        "twenty-seventh" => 27, 
                        "twenty-eighth" => 28, 
                        "twenty-ninth" => 29,
                        "thirtieth" => 30, 
                        "thirty-first" => 31,
                        _ => panic!("Unknow match"),
                    };
                    OrdinalValue { value: value }
                }
           };

           let _ = rule! {
                "<number> <ordinal>",
                (
                    integer_check!(10, 90, vec![|integer: IntegerValue| integer.value % 10 == 0]),
                    ordinal_check!(vec![|ordinal: OrdinalValue| 1 <= ordinal.value && ordinal.value <= 9])
                ),
                |(integer, ordinal)| OrdinalValue { value: integer.0.value + ordinal.0.value }
           };

           let _ = rule! {
                "ordinal (digits)",
                (reg!(r#""0*(\d+) ?(st|nd|rd|th)""#)),
                |text_match| {
                    let value: i64 = text_match.0[1].parse().unwrap();
                    OrdinalValue { value: value }
                }
           };

           let _ = rule! {
                "the <ordinal>",
                (
                    reg!(r#"the"#),
                    ordinal_check!()
                ),
                |(_, ordinal)| ordinal.0
           };
        }
    }
}
