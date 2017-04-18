#![allow(dead_code)] //FIXME

#[macro_use]
extern crate error_chain;
extern crate regex;
extern crate smallvec;

#[macro_use]
pub mod core;
pub mod ontology;

pub mod errors {
    error_chain! {
        foreign_links {
            Regex(::regex::Error);
        }
    }
}
