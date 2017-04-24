use ::*;
use ::NumberValue::*;
use core::rule::rule_errors::*;

pub fn compose_numbers(a: &NumberValue, b:&NumberValue) -> RuleResult<NumberValue> {
    let grain = a.grain().unwrap_or(0) as u32;
    if 10u64.pow(grain) as f32 > b.value() {
        return match (a, b) {
            (&Integer(ref lhs), &Integer(ref rhs)) => {
                Ok(NumberValue::Integer(IntegerValue::new(lhs.value + rhs.value)?))
            },
            _ => {
                Ok(NumberValue::Float(FloatValue { value: a.value() + b.value(), .. FloatValue::default() }))
            },
        }
    } else {
        return Err(RuleErrorKind::Invalid.into())
    }
}
