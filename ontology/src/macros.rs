macro_rules! b {
    ($a:expr) => (Box::new($a))
}

macro_rules! integer_check {
    () => ( $crate::duckling::core::AnyNodePattern::<IntegerValue>::new());
    ($min:expr) => ( $crate::duckling::core::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min)]) );
    ($min:expr, $max:expr) => ( $crate::duckling::core::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min && integer.value <= $max)]) );
    ($min:expr, $max:expr, $predicate:expr) => ( $crate::duckling::core::FilterNodePattern::<IntegerValue>::filter(vec![b!(|integer: &IntegerValue| integer.value >= $min && integer.value <= $max), b!($predicate)]) );
}

macro_rules! integer_filter {
    ($predicate:expr) => ( $crate::duckling::core::FilterNodePattern::<IntegerValue>::filter(vec![b!($predicate)]) );
}

macro_rules! number_check {
    () => ( $crate::duckling::core::AnyNodePattern::<NumberValue>::new() );
    ($predicate:expr) => ( $crate::duckling::core::FilterNodePattern::<NumberValue>::filter(vec![b!($predicate)]) );
}

macro_rules! ordinal_check {
    () => ( $crate::duckling::core::AnyNodePattern::<OrdinalValue>::new() );
    ($predicate:expr) => ( $crate::duckling::core::FilterNodePattern::<OrdinalValue>::filter(vec![b!($predicate)]) );
}

macro_rules! regex {
    ($pattern:expr) => ( reg!(Dimension, $pattern))
}

macro_rules! regex_neg_lh {
    ($pattern:expr, $neg_lh:expr) => ( reg_neg_lh!(Dimension, $pattern, $neg_lh))
}

macro_rules! temperature_check {
    () => ( $crate::duckling::core::AnyNodePattern::<TemperatureValue>::new() );
}

macro_rules! example {
    ($v:expr, $check:expr, $($ex:expr),*) => {
        $( $v.push($crate::duckling::Example::new($ex, Box::new($check))); )*
    };
}
