use duckling::*;
use duckling::train::*;
use super::*;

// ONTOLOGY
#[derive(Debug)]
struct CheckInteger {
    value: i64,
}

impl Check<Dimension> for CheckInteger {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        IntegerValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_integer(v: i64) -> CheckInteger {
    CheckInteger { value: v }
}

#[derive(Debug)]
struct CheckOrdinal {
    value: i64,
}

impl Check<Dimension> for CheckOrdinal {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        OrdinalValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_ordinal(v: i64) -> CheckOrdinal {
    CheckOrdinal { value: v }
}

#[derive(Debug)]
struct CheckFloat {
    value: f32,
}

impl Check<Dimension> for CheckFloat {
    fn check(&self, pn: &ParsedNode<Dimension>) -> bool {
        FloatValue::attempt_from(pn.value.clone())
            .map(|v| v.value == self.value)
            .unwrap_or(false)
    }
}

fn check_float(v: f32) -> CheckFloat {
    CheckFloat { value: v }
}

macro_rules! example {
    ($v:expr, $check:expr, $($ex:expr),*) => {
        $( $v.push(Example::new($ex, Box::new($check))); )*
    };
}

pub fn examples_en_numbers() -> Vec<Example<Dimension>> {
    let mut v = vec![];
    example!(v, check_integer(0), "0", "naught", "nought", "zero", "nil");
    example!(v, check_integer(1), "1", "one", "single");
    example!(v, check_integer(2), "2", "two", "a pair");
    example!(v, check_integer(33), "33", "thirty three", "0033");
    example!(v, check_integer(14), "14", "fourteen");
    example!(v, check_integer(16), "16", "sixteen");
    example!(v, check_integer(17), "17", "seventeen");
    example!(v, check_integer(18), "18", "eighteen");
    example!(v, check_float(1.1), "1.1", "1.10", "01.10");
    example!(v, check_float(0.77), "0.77", ".77");
    example!(v,
             check_integer(100000),
             "100,000",
             "100000",
             "100K",
             "100k");
    example!(v,
             check_integer(3000000),
             "3M",
             "3000K",
             "3000000",
             "3,000,000");
    example!(v,
             check_integer(1200000),
             "1,200,000",
             "1200000",
             "1.2M",
             "1200K",
             ".0012G");
    example!(v,
             check_integer(-1200000),
             "- 1,200,000",
             "-1200000",
             "minus 1,200,000",
             "negative 1200000",
             "-1.2M",
             "-1200K",
             "-.0012G");
    example!(v, check_integer(5000), "5 thousand", "five thousand");
    example!(v, check_integer(122), "one twenty two");
    example!(v, check_integer(200000), "two hundred thousand");
    example!(v, check_integer(21011), "twenty-one thousand eleven");
    example!(v,
             check_integer(721012),
             "seven hundred twenty-one thousand twelve",
             "seven hundred twenty-one thousand and twelve");
    example!(v,
             check_integer(31256721),
             "thirty-one million two hundred fifty-six thousand seven hundred twenty-one");
    example!(v, check_ordinal(4), "the 4th", "4th", "fourth");
    example!(v, check_ordinal(3), "the 3rd", "3rd", "third");
    example!(v, check_ordinal(2), "the 2nd", "2nd", "second");
    example!(v, check_ordinal(21), "the twenty first");
    v
}
