extern crate duckling;
#[macro_use]
extern crate duckling_core as core;
extern crate duckling_ml as ml;

use core::AttemptFrom;
#[macro_use]
mod macros;
mod helpers;
mod examples;
pub mod en;
pub mod parser;

use core::rule::rule_errors::*;

duckling_value! { Dimension
    Number(NumberValue),
    Ordinal(OrdinalValue),
    Temperature(TemperatureValue),
}

impl duckling::Value for Dimension {
    fn same_dimension_as(&self, other: &Self) -> bool {
        match (self, other) {
            (&Dimension::Number(_), &Dimension::Number(_))
                | (&Dimension::Ordinal(_), &Dimension::Ordinal(_))
                | (&Dimension::Temperature(_), &Dimension::Temperature(_)) => true,
            _ => false
        }
    }
}

impl ::std::fmt::Display for Dimension {
    fn fmt(&self, fmt:&mut ::std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        match self {
            &Dimension::Number(_) => write!(fmt, "Number"),
            &Dimension::Ordinal(_) => write!(fmt, "Ordinal"),
            &Dimension::Temperature(_) => write!(fmt, "Temperature"),
        }
    }
}

#[derive(Debug,PartialEq,Copy,Clone)]
pub struct OrdinalValue {
    value: i64,
}

#[derive(Debug,PartialEq,Copy,Clone)]
pub enum Precision {
    Approximate,
    Exact,
}

impl Default for Precision {
    fn default() -> Precision {
        Precision::Exact
    }
}

#[derive(Debug,PartialEq,Clone,Default)]
pub struct IntegerValue {
    value: i64,
    grain: Option<u8>,
    group: bool,
    prefixed: bool,
    suffixed: bool,
    precision: Precision,
}

impl IntegerValue {
    pub fn new(value:i64) -> RuleResult<IntegerValue> {
        Ok(IntegerValue { value: value, grain: None, .. IntegerValue::default() })
    }
    pub fn new_with_grain(value:i64, grain:u8) -> RuleResult<IntegerValue> {
        Ok(IntegerValue { value: value, grain: Some(grain), .. IntegerValue::default() })
    }
}

impl From<IntegerValue> for Dimension {
    fn from(v: IntegerValue) -> Dimension {
        Dimension::Number(NumberValue::Integer(v))
    }
}

impl From<FloatValue> for Dimension {
    fn from(v: FloatValue) -> Dimension {
        Dimension::Number(NumberValue::Float(v))
    }
}

impl From<IntegerValue> for NumberValue {
    fn from(v: IntegerValue) -> NumberValue {
        NumberValue::Integer(v)
    }
}

impl AttemptFrom<Dimension> for IntegerValue {
    fn attempt_from(v: Dimension) -> Option<IntegerValue> {
        if let Dimension::Number(value) = v {
            if let NumberValue::Integer(integer) = value {
                Some(integer)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl AttemptFrom<Dimension> for FloatValue {
    fn attempt_from(v: Dimension) -> Option<FloatValue> {
        if let Dimension::Number(value) = v {
            if let NumberValue::Float(float) = value {
                Some(float)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct FloatValue {
    value: f32,
    prefixed: bool,
    suffixed: bool,
    precision: Precision,
}

impl FloatValue {
    fn default() -> FloatValue {
        FloatValue {
            value: 0.0,
            prefixed: false,
            suffixed: false,
            precision: Precision::Exact,
        }
    }
}

impl From<FloatValue> for NumberValue {
    fn from(v: FloatValue) -> NumberValue {
        NumberValue::Float(v)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberValue {
    Float(FloatValue),
    Integer(IntegerValue),
}

impl NumberValue {
    pub fn prefixed(&self) -> bool {
        match self {
            &NumberValue::Float(ref v) => v.prefixed,
            &NumberValue::Integer(ref v) => v.prefixed,
        }
    }

    pub fn suffixed(&self) -> bool {
        match self {
            &NumberValue::Float(ref v) => v.suffixed,
            &NumberValue::Integer(ref v) => v.suffixed,
        }
    }

    pub fn value(&self) -> f32 {
        match self {
            &NumberValue::Float(ref v) => v.value,
            &NumberValue::Integer(ref v) => v.value as f32,
        }
    }

    pub fn grain(&self) -> Option<u8> {
        match self {
            &NumberValue::Float(_) => None,
            &NumberValue::Integer(ref v) => v.grain
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct TemperatureValue {
    value: f32,
    unit: Option<&'static str>,
    latent: bool

}
