use core::AttemptFrom;
#[macro_use]
mod macros;
pub mod en;

use core::rule::rule_errors::*;

#[derive(Debug,Clone)]
pub enum Dimension {
    Number(NumberValue),
    Ordinal(OrdinalValue),
    Temperature(TemperatureValue),
}

#[derive(Debug,PartialEq,Copy,Clone)]
pub struct OrdinalValue {
    value: i64,
}

impl From<OrdinalValue> for Dimension {
    fn from(v: OrdinalValue) -> Dimension {
        Dimension::Ordinal(v)
    }
}

impl AttemptFrom<Dimension> for OrdinalValue {
    fn attempt_from(v: Dimension) -> Option<OrdinalValue> {
        if let Dimension::Ordinal(value) = v {
            Some(value)
        } else {
            None
        }
    }
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

#[derive(Debug,PartialEq,Clone)]
pub struct FloatValue {
    value: f64,
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

impl From<FloatValue> for Dimension {
    fn from(v: FloatValue) -> Dimension {
        Dimension::Number(NumberValue::Float(v))
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
    fn prefixed(&self) -> bool {
        match self {
            &NumberValue::Float(ref v) => v.prefixed,
            &NumberValue::Integer(ref v) => v.prefixed,
        }
    }

    fn suffixed(&self) -> bool {
        match self {
            &NumberValue::Float(ref v) => v.suffixed,
            &NumberValue::Integer(ref v) => v.suffixed,
        }
    }

    fn value(&self) -> f64 {
        match self {
            &NumberValue::Float(ref v) => v.value,
            &NumberValue::Integer(ref v) => v.value as f64,
        }
    }
}

impl AttemptFrom<Dimension> for NumberValue {
    fn attempt_from(v: Dimension) -> Option<NumberValue> {
        if let Dimension::Number(value) = v {
            Some(value)
        } else {
            None
        }
    }
}

impl From<NumberValue> for Dimension {
    fn from(v: NumberValue) -> Dimension {
        Dimension::Number(v)
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct TemperatureValue {
    value: f64,
    unit: Option<&'static str>,
    latent: bool

}

impl AttemptFrom<Dimension> for TemperatureValue {
    fn attempt_from(v: Dimension) -> Option<TemperatureValue> {
        if let Dimension::Temperature(value) = v {
            Some(value)
        } else {
            None
        }
    }
}

impl From<TemperatureValue> for Dimension {
    fn from(v: TemperatureValue) -> Dimension {
        Dimension::Temperature(v)
    }
}
