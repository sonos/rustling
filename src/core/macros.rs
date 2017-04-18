
macro_rules! reg {
    ($typ:ty, $pattern:expr) => ( $crate::core::pattern::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $pattern) )
}

macro_rules! rule {
    ($name:expr, ($a:expr), $f:expr) =>
        { Box::new($crate::core::rule::Rule1::new($name, $a, $f)) };
    ($name:expr, ($a:expr, $b:expr), $f:expr) =>
        { Box::new($crate::core::rule::Rule2::new($name, ($a, $b), $f)) };
    ($name:expr, ($a:expr, $b:expr, $c:expr), $f:expr) => 
        { Box::new($crate::core::rule::Rule3::new($name, ($a, $b, $c), $f)) };
}

macro_rules! dim {
    ($typ:ty) => ( $crate::core::pattern::AnyNodePattern::<$typ>::new() );
    ($typ:ty, $predicates:expr) => ( $crate::core::pattern::FilterNodePattern::<$typ>::filter($predicates) );
}
