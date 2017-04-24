#[macro_export]
macro_rules! rustling_value {
    ( $name:ident $($varname:ident($varty:ty)),*, ) => {
        #[derive(Debug,Clone, PartialEq)]
        pub enum $name {
            $( $varname($varty) ),*
        }

        $(
            impl From<$varty> for $name {
                fn from(v: $varty) -> $name {
                    $name::$varname(v)
                }
            }
            impl AttemptFrom<$name> for $varty {
                fn attempt_from(v: $name) -> Option<$varty> {
                    if let $name::$varname(value) = v {
                        Some(value)
                    } else {
                        None
                    }
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! reg {
    ($typ:ty, $pattern:expr) => ( $crate::core::TextPattern::<$typ>::new($crate::regex::Regex::new($pattern)?, $pattern) )
}

#[macro_export]
macro_rules! reg_neg_lh {
    ($typ:ty, $pattern:expr, $neg_lh:expr) => {
        $crate::core::TextNegLHPattern::<$typ>::new(
            reg!($typ, $pattern),
            $crate::regex::Regex::new($neg_lh)?,
            concat!($pattern, "(?=", $neg_lh, ")") )
    }
}


#[macro_export]
macro_rules! rule {
    ($name:expr, ($a:expr), $f:expr) =>
        { Box::new($crate::core::Rule1::new($name, $a, $f)) };
    ($name:expr, ($a:expr, $b:expr), $f:expr) => {
        Box::new($crate::core::Rule2::new($name, ($a, $b), $f)) };
    ($name:expr, ($a:expr, $b:expr, $c:expr), $f:expr) => 
        { Box::new($crate::core::Rule3::new($name, ($a, $b, $c), $f)) };
}

#[macro_export]
macro_rules! dim {
    ($typ:ty) => ( $crate::core::AnyNodePattern::<$typ>::new() );
    ($typ:ty, $predicates:expr) => ( $crate::core::FilterNodePattern::<$typ>::filter($predicates) );
}
