use core::*;
use errors::*;
use ontology::*;
use ontology::Precision::*;

fn rules<'a>() -> Result<RuleSet<'a, Dimension>> {
    Ok(RuleSet(vec![
        rule! { "ten", (regex!(r#"ten"#)), |_| IntegerValue::new_with_grain(10, 1) },
        rule! { "single", (regex!(r#"single"#)), |_| IntegerValue::new_with_grain(1, 1) },
        rule! { "a pair", (regex!(r#"a pair( of)?"#)), |_| IntegerValue::new_with_grain(2, 1) },
        rule! { "dozen", (regex!(r#"dozen"#)), |_| IntegerValue::new_with_grain(12, 1) },
        rule! { "hundred", (regex!(r#"hundreds?"#)), |_| IntegerValue::new_with_grain(100, 2) },
        rule! { "thousand", (regex!(r#"thousands?"#)), |_| IntegerValue::new_with_grain(1000, 3) },
        rule! { "million", (regex!(r#"millions?"#)), |_| IntegerValue::new_with_grain(1000000, 6) },
        rule! { "couple", (regex!(r#"(a )?couple( of)?"#)), |_| IntegerValue::new_with_grain(2, 1) },
        rule! { "few", (regex!(r#"(a )?few"#)), 
            |_| Ok(IntegerValue { value: 3, grain:Some(1), precision: Approximate, .. IntegerValue::default() })
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
                IntegerValue::new_with_grain(value, 1) }
            },
        rule! {
            "integer 21..99",
            (
                integer_check!(10, 90, |integer: &IntegerValue| integer.value % 10 == 0),
                integer_check!(1, 9)
            ),
            |a, b| IntegerValue::new(a.value.value + b.value.value)
        },
        rule! {
            "integer (numeric)",
            (regex!(r#"(\d{1,18})"#)),
            |text_match| {
                let value: i64 = text_match.0[0].parse().unwrap();
                IntegerValue::new(value)
            }
        },
        rule! {
            "integer with thousands separator ,",
            (regex!(r#"(\d{1,3}(,\d\d\d){1,5})"#)),
            |text_match| {
                let reformatted_string = text_match.0[1].replace(",", "");
                let value: i64 = reformatted_string.parse().unwrap();
                IntegerValue::new(value)
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
                IntegerValue::new_with_grain(value, 1) }
            },
        rule! {
            "number dozen",
            (
                integer_check!(1, 10),
                integer_filter!(|integer: &IntegerValue| integer.group)
            ),
            |a, b| Ok(IntegerValue { value: a.value.value * b.value.value, grain:b.value.grain, ..IntegerValue::default() })
        },
        rule! {
             "number hundreds",
             (
                 integer_check!(1, 99),
                 integer_check!(100, 100)
             ),
             |a, b| Ok(IntegerValue { value: a.value.value * b.value.value, grain:b.value.grain, ..IntegerValue::default() })
        },
        rule! {
             "number thousands",
             (
                 integer_check!(1, 999),
                 integer_check!(1000, 1000)
             ),
             |a, b| Ok(IntegerValue { value: a.value.value * b.value.value, grain:b.value.grain, ..IntegerValue::default() })
        },
        rule! {
             "number millions",
             (
                 integer_check!(1, 99),
                 integer_check!(1000000, 1000000)
             ),
             |a, b| Ok(IntegerValue { value: a.value.value * b.value.value, grain:b.value.grain, ..IntegerValue::default() })
        },
        rule! {
             "decimal number",
             (regex!(r#"(\d*\.\d+)"#)),
             |text_match| {
                 let value: f64 = text_match.0[0].parse().unwrap();
                 Ok(FloatValue { value: value, .. FloatValue::default() })
             }
        },
        rule! {
             "number dot number",
             (
                 number_check!(|number: &NumberValue| !number.prefixed()),
                 regex!(r#"dot|point"#),
                 number_check!(|number: &NumberValue| !number.suffixed())
             ),
             |a, _, b| Ok(FloatValue { value: b.value.value() * 0.1 + a.value.value(), .. FloatValue::default() })
        },
        rule! {
             "decimal with thousands separator",
             (regex!(r#"(\d+(,\d\d\d)+\.\d+)"#)),
             |text_match| {
                 let reformatted_string = text_match.0[1].replace(",", "");
                 let value: f64 = reformatted_string.parse().unwrap();
                 Ok(FloatValue { value: value, .. FloatValue::default() })
             }
        },
        rule! {
            "numbers prefix with -, negative or minus",
            (
                regex!(r#"-|minus\s?|negative\s?"#),
                number_check!(|number: &NumberValue| !number.prefixed())
            ),
            |_, a| -> RuleResult<NumberValue> {
                Ok(match a.value.clone() { // checked
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
                })
            }
        },
        rule! {
            "numbers suffixes (K, M, G)",
            (
                number_check!(|number: &NumberValue| !number.suffixed()),
                regex!(r#"([kmg])(?=[\W\$€]|$)"#)
            ),
            |a, text_match| -> RuleResult<NumberValue> {
                let multiplier = match text_match.0[0].as_ref() {
                    "k" => 1000,
                    "m" => 1000000,
                    "g" => 1000000000,
                    _ => panic!("Unknown match"),
                };
                Ok(match a.value.clone() { // checked
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
                })
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
                 Ok(OrdinalValue { value: value })
             }
        },
        rule! {
            "<number> <ordinal>",
             (
                 integer_check!(10, 90, |integer: &IntegerValue| integer.value % 10 == 0),
                 ordinal_check!(|ordinal: &OrdinalValue| 1 <= ordinal.value && ordinal.value <= 9)
             ),
             |integer, ordinal| Ok(OrdinalValue { value: integer.value.value + ordinal.value.value })
        },
        rule! {
            "ordinal (digits)",
            (regex!(r#""0*(\d+) ?(st|nd|rd|th)""#)),
            |text_match| {
                let value: i64 = text_match.0[1].parse().unwrap();
                Ok(OrdinalValue { value: value })
            }
        },
        rule! {
            "the <ordinal>",
            (
                regex!(r#"the"#),
                ordinal_check!()
            ),
            |_, ordinal| Ok(ordinal.value)
        }
    ]))
}
