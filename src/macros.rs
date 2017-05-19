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
    ( #[$doc:meta] #[$derive:meta] $name:ident $kindname:ident { $($varname:ident($varty:ty)),*, } fn latent($v1:ident: &$t1:ty) -> bool { $( $body1:tt )* } fn extract_payload($v2:ident: &$t2:ty) -> Option<$payload:ty> { $( $body2:tt )* } ) => {
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

            fn latent(&self) -> bool {
                #[allow(unused_variables)]
                fn i($v1: &$t1) -> bool {
                    $( $body1 )*
                }
                i(&self)
            }
        }

        impl NodePayload for $name {
            type Payload = $payload;
            fn extract_payload(&self) -> Option<Self::Payload> {
                #[allow(unused_variables)]
                fn i($v2: &$t2) -> Option<$payload> {
                    $( $body2 )*
                }
                i(&self)
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

        $( 
            variant_converters!($name, $varname, $varty); 
            
            impl NodePayload for $varty {
                type Payload = $payload;
                fn extract_payload(&self) -> Option<Self::Payload> {
                    $name::from(self.clone()).extract_payload()
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! dim {
    ($typ:ty) => ( $crate::core::AnyNodePattern::<$typ>::new() );
    ($typ:ty, $predicates:expr) => ( $crate::core::FilterNodePattern::<$typ>::filter($predicates) );
}
