
#[macro_export]
macro_rules! reg {
    ($typ:ty, $pattern:expr) => ( $crate::pattern::TextPattern::<$typ>::new($crate::regex::Regex::new($pattern)?, $pattern) )
}

#[macro_export]
macro_rules! reg_neg_lh {
    ($typ:ty, $pattern:expr, $neg_lh:expr) => {
        $crate::pattern::TextNegLHPattern::<$typ>::new(
            reg!($typ, $pattern),
            $crate::regex::Regex::new($neg_lh)?,
            concat!($pattern, "(?=", $neg_lh, ")") )
    }
}


#[macro_export]
macro_rules! rule {
    ($name:expr, ($a:expr), $f:expr) =>
        { Box::new($crate::rule::Rule1::new($name, $a, $f)) };
    ($name:expr, ($a:expr, $b:expr), $f:expr) => {
        Box::new($crate::rule::Rule2::new($name, ($a, $b), $f)) };
    ($name:expr, ($a:expr, $b:expr, $c:expr), $f:expr) => 
        { Box::new($crate::rule::Rule3::new($name, ($a, $b, $c), $f)) };
}

#[macro_export]
macro_rules! dim {
    ($typ:ty) => ( $crate::pattern::AnyNodePattern::<$typ>::new() );
    ($typ:ty, $predicates:expr) => ( $crate::pattern::FilterNodePattern::<$typ>::filter($predicates) );
}
