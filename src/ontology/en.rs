use core::*;
use ontology::*;
use ontology::Precision::*;

fn rules<'a>() -> RuleSet<'a, Dimension> {
    RuleSet(vec![
        rule! {
            "ten",
            (regex!(r#"ten"#)),
            |_| IntegerValue { value: 10, grain: Some(1), .. IntegerValue::default() }
        },
        rule! {
            "ten",
            (regex!(r#"ten"#)),
            |_| IntegerValue { value: 10, grain: Some(1), .. IntegerValue::default() }
        },
        rule! {
            "single",
            (regex!(r#"single"#)),
            |_| IntegerValue { value: 1, grain: Some(1), .. IntegerValue::default()  }
        },
        rule! {
            "a pair",
            (regex!(r#"a pair( of)?"#)),
            |_| IntegerValue { value: 2, grain: Some(1), .. IntegerValue::default() }
        },
        rule! {
            "dozen",
            (regex!(r#"dozen"#)),
            |_| IntegerValue { value: 12, grain: Some(1), .. IntegerValue::default()  }
        },
        rule! {
            "hundred",
            (regex!(r#"hundreds?"#)),
            |_| IntegerValue { value: 100, grain: Some(2), .. IntegerValue::default()  }
        },
        rule! {
            "thousand",
            (regex!(r#"thousands?"#)),
            |_| IntegerValue { value: 1000, grain: Some(3), .. IntegerValue::default()  }
        },
        rule! {
            "million",
            (regex!(r#"millions?"#)),
            |_| IntegerValue { value: 1000000, grain: Some(6), .. IntegerValue::default() }
        },
        rule! {
            "couple",
            (regex!(r#"(a )?couple( of)?"#)),
            |_| IntegerValue { value: 2, grain: Some(1), .. IntegerValue::default() }
        },
        rule! {
            "few",
            (regex!(r#"(a )?few"#)),
            |_| IntegerValue { value: 3, grain: Some(1), precision: Approximate, .. IntegerValue::default() }
        },
        rule! {
            "integer (20..90)",
            (regex!(r#"(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"#)),
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
                IntegerValue { value: value, grain: Some(1), .. IntegerValue::default() }
            }
        },
        rule! {
            "integer 21..99",
            (
                integer_check!(10, 90, |integer: &IntegerValue| integer.value % 10 == 0),
                integer_check!(1, 9)
            ),
            |a, b| IntegerValue { value: a.value.value + b.value.value, .. IntegerValue::default() }
        },
        rule! {
            "integer (numeric)",
            (regex!(r#"(\d{1,18})"#)),
            |text_match| {
                let value: i64 = text_match.0[0].parse().unwrap();
                IntegerValue { value: value, .. IntegerValue::default() }
            }
        },
        rule! {
            "integer with thousands separator ,",
            (regex!(r#"(\d{1,3}(,\d\d\d){1,5})"#)),
            |text_match| {
                let reformatted_string = text_match.0[1].replace(",", "");
                let value: i64 = reformatted_string.parse().unwrap();
                IntegerValue { value: value, .. IntegerValue::default() }
            }
        },
        rule! {
            "special composition for missing hundreds like in one twenty two",
            (
                integer_check!(1, 9),
                integer_check!(10, 99)
            ),
            |a, b| {
                let value = a.value.value * 100 + b.value.value;
                IntegerValue { value: value, grain: Some(1), .. IntegerValue::default() }
            }
        },
        rule! {
            "number dozen",
            (
                integer_check!(1, 10),
                integer_filter!(|integer: &IntegerValue| integer.group)
            ),
            |a, b| IntegerValue { value: a.value.value * b.value.value, grain: b.value.grain, .. IntegerValue::default() }
        },
        rule! {
             "number hundreds",
             (
                 integer_check!(1, 99),
                 integer_check!(100, 100)
             ),
             |a, b| IntegerValue { value: a.value.value * b.value.value, grain: b.value.grain, .. IntegerValue::default() }
        },
        rule! {
             "number thousands",
             (
                 integer_check!(1, 999),
                 integer_check!(1000, 1000)
             ),
             |a, b| IntegerValue { value: a.value.value * b.value.value, grain: b.value.grain, .. IntegerValue::default() }
        },
        rule! {
             "number millions",
             (
                 integer_check!(1, 99),
                 integer_check!(1000000, 1000000)
             ),
             |a, b| IntegerValue { value: a.value.value * b.value.value, grain: b.value.grain, .. IntegerValue::default() }
        },
        rule! {
             "decimal number",
             (regex!(r#"(\d*\.\d+)"#)),
             |text_match| {
                 let value: f64 = text_match.0[0].parse().unwrap();
                 FloatValue { value: value, .. FloatValue::default() }
             }
        },
        rule! {
             "number dot number",
             (
                 number_check!(|number: &NumberValue| !number.prefixed()),
                 regex!(r#"dot|point"#),
                 number_check!(|number: &NumberValue| !number.suffixed())
             ),
             |a, _, b| FloatValue { value: b.value.value() * 0.1 + a.value.value(), .. FloatValue::default() }
        },
        rule! {
             "decimal with thousands separator",
             (regex!(r#"(\d+(,\d\d\d)+\.\d+)"#)),
             |text_match| {
                 let reformatted_string = text_match.0[1].replace(",", "");
                 let value: f64 = reformatted_string.parse().unwrap();
                 FloatValue { value: value, .. FloatValue::default() }
             }
        },
        rule! {
            "numbers prefix with -, negative or minus",
            (
                regex!(r#"-|minus\s?|negative\s?"#),
                number_check!(|number: &NumberValue| !number.prefixed())
            ),
            |_, a| -> NumberValue {
                match a.value.clone() {
                    NumberValue::Integer(integer) => IntegerValue {
                                                        value: integer.value * -1,
                                                        prefixed: true,
                                                        .. integer
                                                    }.into(),
                    NumberValue::Float(float) => FloatValue {
                                                        value: float.value * -1.0,
                                                        prefixed: true,
                                                        .. float
                                                    }.into(),
                }
            }
        },
        rule! {
            "numbers suffixes (K, M, G)",
            (
                number_check!(|number: &NumberValue| !number.suffixed()),
                regex!(r#"([kmg])(?=[\W\$€]|$)"#)
            ),
            |a, text_match| -> NumberValue {
                let multiplier = match text_match.0[0].as_ref() {
                    "k" => 1000,
                    "m" => 1000000,
                    "g" => 1000000000,
                    _ => panic!("Unknown match"),
                };
                match a.value.clone() {
                    NumberValue::Integer(integer) => IntegerValue {
                                                        value: integer.value * multiplier,
                                                        suffixed: true,
                                                        .. integer
                                                    }.into(),
                    NumberValue::Float(float) => FloatValue {
                                                        value: float.value * (multiplier as f64),
                                                        suffixed: true,
                                                        .. float
                                                    }.into(),
                }
            }
        },
        rule! {
             "ordinals (first..31st)",
             (regex!(r#"(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth|seventeenth|eighteenth|nineteenth|twentieth|twenty-first|twenty-second|twenty-third|twenty-fourth|twenty-fifth|twenty-sixth|twenty-seventh|twenty-eighth|twenty-ninth|thirtieth|thirty-first)"#)),
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
        },
        rule! {
            "<number> <ordinal>",
             (
                 integer_check!(10, 90, |integer: &IntegerValue| integer.value % 10 == 0),
                 ordinal_check!(|ordinal: &OrdinalValue| 1 <= ordinal.value && ordinal.value <= 9)
             ),
             |integer, ordinal| OrdinalValue { value: integer.value.value + ordinal.value.value }
        },
        rule! {
            "ordinal (digits)",
            (regex!(r#""0*(\d+) ?(st|nd|rd|th)""#)),
            |text_match| {
                let value: i64 = text_match.0[1].parse().unwrap();
                OrdinalValue { value: value }
            }
        },
        rule! {
            "the <ordinal>",
            (
                regex!(r#"the"#),
                ordinal_check!()
            ),
            |_, ordinal| ordinal.value
        }
    ])
}
