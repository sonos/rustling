#[macro_use]
extern crate clap;
extern crate duckling_ontology;
#[macro_use] extern crate prettytable;
use prettytable::Table;
use prettytable::row::Row;
use prettytable::cell::Cell;

fn main() {
    let matches = clap_app!(duckling_cli =>
        (@subcommand parse =>
             (@arg sentence: +required "Sentence to test")
        )
    ).get_matches();

    match matches.subcommand() {
        ("parse", Some(matches)) => {
            let sentence = matches.value_of("sentence").unwrap();
            let parser = duckling_ontology::parser::build_parser_en().unwrap();
            let candidates = parser.candidates(&sentence, |_| Some(12)).unwrap();
            let mut table = Table::new();
            for (ix,c) in candidates.iter().enumerate().rev() {
                let mut hilite = String::new();
                for _ in 0..c.1.range.0 {
                    hilite.push('_');
                }
                hilite.push_str(&sentence[c.1.range.0..c.1.range.1]);
                for _ in c.1.range.1..sentence.len() {
                    hilite.push('_');
                }
                table.add_row(row![ix, if c.3 { "*" } else { " "}, hilite, format!("{}", c.1.value), c.0.root_node.rule_name]);
            }
            table.printstd();
        }
        (cmd, _) => panic!("Unknown command {}")
    }
}
