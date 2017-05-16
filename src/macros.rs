#[macro_export]
macro_rules! variant_converters {
    ($name:ident, $varname:ident, $varty:ty) => {
        impl From<$varty> for $name {
            fn from(v: $varty) -> $name {
                $name::$varname(v)
            }
        }

        impl $crate::AttemptFrom<$name> for $varty {
            fn attempt_from(v: $name) -> Option<$varty> {
                if let $name::$varname(value) = v {
                    Some(value)
                } else {
                    None
                }
            }
        }
    }
}

#[macro_export]  
macro_rules! rustling_value {
    ( #[$doc:meta] #[$derive:meta] $name:ident $kindname:ident { $($varname:ident($varty:ty)),*, } fn intermediate(&self) -> bool { $( $body:tt )* } ) => {
        #[$doc] #[$derive]
        pub enum $name {
            $( $varname($varty) ),*
        }

        #[derive(Debug,Copy,Clone,PartialEq)]
        pub enum $kindname {
            $( $varname ),*
        }

        impl Value for $name {
            type Kind = $kindname;
            fn kind(&self) -> Self::Kind {
                match self {
                    $(
                        &$name::$varname(_) => $kindname::$varname,
                    )*
                }
            }

            fn intermediate(&self) -> bool {
                $( $body )*
            }
        }

        impl ::std::str::FromStr for $kindname {
            type Err=String;
            fn from_str(s: &str) -> ::std::result::Result<$kindname, Self::Err> {
                match s {
                    $(
                        stringify!($varname) => Ok($kindname::$varname),
                    )*
                    _ => Err(format!("{} is not a known {}", s, stringify!($kindname)))
                }
            }
        }

        impl ::std::string::ToString for $kindname {
            fn to_string(&self) -> String {
                match self {
                    $(
                        &$kindname::$varname => stringify!($varname).to_string(),
                    )*
                }
            }
        }

        $( variant_converters!($name, $varname, $varty); )*
    }
}

#[macro_export]
macro_rules! dim {
    ($typ:ty) => ( $crate::core::AnyNodePattern::<$typ>::new() );
    ($typ:ty, $predicates:expr) => ( $crate::core::FilterNodePattern::<$typ>::filter($predicates) );
}
