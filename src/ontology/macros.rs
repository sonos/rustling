macro_rules! b {
    ($a:expr) => (Box::new($a))
}

macro_rules! integer_check {
    () => ( $crate::core::pattern::AnyNodePattern::<IntegerValue>::new());
    ($min:expr) => ( $crate::core::pattern::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min)]) );
    ($min:expr, $max:expr) => ( $crate::core::pattern::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min && integer.value <= $max)]) );
    ($min:expr, $max:expr, $predicate:expr) => ( $crate::core::pattern::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min && integer.value <= $max), b!($predicate)]) );
}

macro_rules! integer_filter {
    ($predicate:expr) => ( $crate::core::pattern::FilterNodePattern::<IntegerValue>::filter(vec![b!($predicate)]) );
}

macro_rules! number_check {
    () => ( $crate::core::pattern::AnyNodePattern::<NumberValue>::new() );
    ($predicate:expr) => ( $crate::core::pattern::FilterNodePattern::<NumberValue>::filter(vec![b!($predicate)]) );
}

macro_rules! ordinal_check {
    () => ( $crate::core::pattern::AnyNodePattern::<OrdinalValue>::new() );
    ($predicate:expr) => ( $crate::core::pattern::FilterNodePattern::<OrdinalValue>::filter(vec![b!($predicate)]) );
}

macro_rules! regex {
    ($pattern:expr) => ( reg!(Dimension, $pattern))
}

macro_rules! temperature_check {
    () => ( $crate::core::pattern::AnyNodePattern::<TemperatureValue>::new() );
}