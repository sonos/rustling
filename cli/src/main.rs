#[macro_use]
extern crate clap;
extern crate duckling_ontology;
#[macro_use]
extern crate prettytable;
use prettytable::Table;

fn main() {
    let matches = clap_app!(duckling_cli =>
        (@subcommand parse =>
             (@arg sentence: +required "Sentence to test")
        )
    )
        .get_matches();

    match matches.subcommand() {
        ("parse", Some(matches)) => {
            let sentence = matches.value_of("sentence").unwrap().to_lowercase();
            let parser = duckling_ontology::parser::build_parser_en().unwrap();
            let candidates = parser.candidates(&*sentence, |_| Some(12)).unwrap();
            let mut table = Table::new();
            table.set_format(*prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
            table.set_titles(row!["ix", "best", "log(p)", "p", "text", "dim", "rule", "childs"]);
            for (ix, c) in candidates.iter().enumerate().rev() {
                let mut hilite = String::new();
                for _ in 0..c.1.range.0 {
                    hilite.push('_');
                }
                hilite.push_str(&sentence[c.1.range.0..c.1.range.1]);
                for _ in c.1.range.1..sentence.len() {
                    hilite.push('_');
                }
                table.add_row(row![ix,
                                   if c.3 { "*" } else { " " },
                                   c.1.probalog,
                                   f32::exp(c.1.probalog),
                                   hilite,
                                   format!("{}", c.1.value),
                                   c.0.root_node.rule_name,
                                   c.0
                                       .root_node
                                       .children
                                       .iter()
                                       .map(|n| n.rule_name)
                                       .collect::<Vec<_>>()
                                       .join("+")]);
            }
            table.printstd();
        }
        (cmd, _) => panic!("Unknown command {}", cmd),
    }
}
