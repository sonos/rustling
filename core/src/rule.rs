use ::*;
use pattern::*;
use errors::*;
use rule::rule_errors::*;
use smallvec::SmallVec;

pub mod rule_errors {
    error_chain! {
        types {
            RuleError, RuleErrorKind, RuleResultExt, RuleResult;
        }

        foreign_links {
            NumParseInt(::std::num::ParseIntError);
            NumParseFloat(::std::num::ParseFloatError);
            Regex(::regex::Error);
        }

        errors {
            Invalid
        }
    }
}

fn make_production_error(s: RuleError) -> CoreError {
    CoreErrorKind::ProductionRuleError(format!("{:?}", s)).into()

}

macro_rules! svec {
    ($($item:expr),*) => { {
        let mut v = ::smallvec::SmallVec::new();
        $( v.push($item); )*
        v
    }
    }
}

#[derive(Debug)]
pub struct RuleProductionArg<'a, M: Match + 'a> {
    sentence: &'a str,
    match_: &'a M,
}

impl<'a, M: Match> RuleProductionArg<'a, M> {
    pub fn new(sentence: &'a str, match_: &'a M) -> RuleProductionArg<'a, M> {
        RuleProductionArg {
            sentence: sentence,
            match_: match_,
        }
    }
}

impl<'a, V: NodePayload> RuleProductionArg<'a, Text<V>> {
    pub fn group(&self, ix: usize) -> &'a str {
        let g = self.match_.groups[ix];
        &self.sentence[g.0..g.1]
    }
}

impl<'a, V: NodePayload> RuleProductionArg<'a, ParsedNode<V>> {
    pub fn value(&self) -> &V {
        &self.match_.value
    }
}


fn adjacent<A: Match, B: Match>(a: &A, b: &B, sentence: &str) -> bool {
    a.byte_range().1 <= b.byte_range().0 &&
    sentence[a.byte_range().1..b.byte_range().0]
        .chars()
        .all(|c| c.is_whitespace() || c == '-')
}

type ParsedNodes<StashValue> = SmallVec<[ParsedNode<StashValue>; 1]>;

pub trait Rule<StashValue: NodePayload>: Send + Sync {
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>>;
    fn is_terminal(&self) -> bool;
}

pub struct Rule1<PA, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>
{
    sym: Sym,
    pattern: PA,
    production: F,
    is_terminal: bool,
    _phantom: SendSyncPhantomData<(V, StashValue)>,
}

impl<PA, V, StashValue, F> Rule<StashValue> for Rule1<PA, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec![sub.to_node()];
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_sym != self.sym
                            }) {
                    match (self.production)(&RuleProductionArg::new(sentence, sub)) {
                        Ok(v) => {
                          let payload = v.extract_payload();
                            Some(Ok(ParsedNode::new(self.sym,
                                                    v.into(),
                                                    sub.byte_range(),
                                                    payload,
                                                    nodes)))
                        }
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, V, StashValue, F> Rule1<PA, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>
{
    pub fn new(sym: Sym, is_terminal: bool, pat: PA, prod: F) -> Rule1<PA, V, StashValue, F> {
        Rule1 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<PA::M>> {
        self.pattern.predicate(stash, sentence)
    }
}



pub struct Rule2<PA, PB, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F:for<'a>  Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V>,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    sym: Sym,
    is_terminal: bool,
    pattern: (PA, PB),
    production: F,
    _phantom: SendSyncPhantomData<(V,  StashValue)>,
}

impl<PA, PB, V, StashValue, F> Rule<StashValue>
    for Rule2<PA, PB, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches.iter()
            .filter_map(|sub| {
                let nodes = svec![sub.0.to_node(), sub.1.to_node()];
                if stash.iter().all(|old_node| {
                    old_node.root_node.children != nodes ||
                    old_node.root_node.rule_sym != self.sym
                }) {
                    let byte_range = Range(sub.0.byte_range().0, sub.1.byte_range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0), &RuleProductionArg::new(sentence, &sub.1)) {
                        Ok(v) => {
                            let payload = v.extract_payload();
                            Some(Ok(ParsedNode::new(self.sym,
                                                    v.into(),
                                                    byte_range,
                                                    payload,
                                                    nodes)))
                        }
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, PB, V, StashValue, F> Rule2<PA, PB, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>) -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
{
    pub fn new(sym: Sym,
               is_terminal: bool,
               pat: (PA, PB),
               prod: F,)
               -> Rule2<PA, PB, V, StashValue, F> {
        Rule2 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self, stash: &Stash<StashValue>, sentence: &str) -> CoreResult<PredicateMatches<(PA::M, PB::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result)
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    result.push((m0.clone(), m1.clone()))
                }
            }
        }
        Ok(result)
    }
}


pub struct Rule3<PA, PB, PC, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
{
    sym: Sym,
    is_terminal: bool,
    pattern: (PA, PB, PC),
    production: F,
    _phantom: SendSyncPhantomData<(V, StashValue)>,
}

impl<PA, PB, PC, V, StashValue, F> Rule<StashValue> for Rule3<PA, PB, PC, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec!(sub.0.to_node(), sub.1.to_node(), sub.2.to_node());
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_sym != self.sym
                            }) {
                    let byte_range = Range(sub.0.byte_range().0, sub.2.byte_range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0),
                                            &RuleProductionArg::new(sentence, &sub.1),
                                            &RuleProductionArg::new(sentence, &sub.2)) {
                        Ok(v) => {
                          let payload = v.extract_payload();
                          Some(Ok(ParsedNode::new(
                                        self.sym, 
                                        v.clone().into(), 
                                        byte_range, 
                                        payload, 
                                        nodes)
                              ))
                        },
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, PB, PC, V, StashValue, F> Rule3<PA, PB, PC, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>
{
    pub fn new(sym: Sym, is_terminal: bool, pat: (PA, PB, PC), prod: F) -> Rule3<PA, PB, PC, V, StashValue, F> {
        Rule3 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<(PA::M, PB::M, PC::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result);
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        if matches_1.is_empty() {
            return Ok(result);
        }
        let matches_2 = self.pattern.2.predicate(stash, sentence)?;
        if matches_2.is_empty() {
            return Ok(result);
        }
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    for m2 in matches_2.iter() {
                        if adjacent(m1, m2, sentence) {
                            result.push((m0.clone(), m1.clone(), m2.clone()))
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}


pub struct Rule4<PA, PB, PC, PD, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
{
    sym: Sym,
    is_terminal: bool,
    pattern: (PA, PB, PC, PD),
    production: F,
    _phantom: SendSyncPhantomData<(V, StashValue)>,
}

impl<PA, PB, PC, PD, V, StashValue, F> Rule<StashValue> for Rule4<PA, PB, PC, PD, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec!(sub.0.to_node(), sub.1.to_node(), sub.2.to_node(), sub.3.to_node());
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_sym != self.sym
                            }) {
                    let byte_range = Range(sub.0.byte_range().0, sub.3.byte_range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0),
                                            &RuleProductionArg::new(sentence, &sub.1),
                                            &RuleProductionArg::new(sentence, &sub.2),
                                            &RuleProductionArg::new(sentence, &sub.3)) {
                        Ok(v) =>  {
                          let payload = v.extract_payload();
                          Some(Ok(ParsedNode::new(
                                        self.sym, 
                                        v.clone().into(), 
                                        byte_range, 
                                        payload, 
                                        nodes)
                          ))
                        },
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, PB, PC, PD, V, StashValue, F> Rule4<PA, PB, PC, PD, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
{
    pub fn new(sym: Sym, is_terminal: bool, pat: (PA, PB, PC, PD), prod: F) -> Rule4<PA, PB, PC, PD, V, StashValue, F> {
        Rule4 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<(PA::M, PB::M, PC::M, PD::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result);
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        if matches_1.is_empty() {
            return Ok(result);
        }
        let matches_2 = self.pattern.2.predicate(stash, sentence)?;
        if matches_2.is_empty() {
            return Ok(result);
        }
        let matches_3 = self.pattern.3.predicate(stash, sentence)?;
        if matches_3.is_empty() {
            return Ok(result);
        }
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    for m2 in matches_2.iter() {
                        if adjacent(m1, m2, sentence) {
                            for m3 in matches_3.iter() {
                                if adjacent(m2, m3, sentence) {
                                    result.push((m0.clone(), m1.clone(), m2.clone(), m3.clone()))
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}

pub struct Rule5<PA, PB, PC, PD, PE, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
{
    sym: Sym,
    is_terminal: bool,
    pattern: (PA, PB, PC, PD, PE),
    production: F,
    _phantom: SendSyncPhantomData<(V, StashValue)>,
}

impl<PA, PB, PC, PD, PE, V, StashValue, F> Rule<StashValue> for Rule5<PA, PB, PC, PD, PE, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec!(sub.0.to_node(), sub.1.to_node(), sub.2.to_node(), sub.3.to_node(), sub.4.to_node());
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_sym != self.sym
                            }) {
                    let byte_range = Range(sub.0.byte_range().0, sub.4.byte_range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0),
                                            &RuleProductionArg::new(sentence, &sub.1),
                                            &RuleProductionArg::new(sentence, &sub.2),
                                            &RuleProductionArg::new(sentence, &sub.3),
                                            &RuleProductionArg::new(sentence, &sub.4)) {
                        Ok(v) => {
                          let payload = v.extract_payload();
                          Some(Ok(ParsedNode::new(
                                        self.sym, 
                                        v.into(), 
                                        byte_range, 
                                        payload, 
                                        nodes))
                          )
                        },
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, PB, PC, PD, PE, V, StashValue, F> Rule5<PA, PB, PC, PD, PE, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
{
    pub fn new(sym: Sym, is_terminal: bool, pat: (PA, PB, PC, PD, PE), prod: F) -> Rule5<PA, PB, PC, PD, PE, V, StashValue, F> {
        Rule5 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<(PA::M, PB::M, PC::M, PD::M, PE::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result);
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        if matches_1.is_empty() {
            return Ok(result);
        }
        let matches_2 = self.pattern.2.predicate(stash, sentence)?;
        if matches_2.is_empty() {
            return Ok(result);
        }
        let matches_3 = self.pattern.3.predicate(stash, sentence)?;
        if matches_3.is_empty() {
            return Ok(result);
        }
        let matches_4 = self.pattern.4.predicate(stash, sentence)?;
        if matches_4.is_empty() {
            return Ok(result);
        }
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    for m2 in matches_2.iter() {
                        if adjacent(m1, m2, sentence) {
                            for m3 in matches_3.iter() {
                                if adjacent(m2, m3, sentence) {
                                    for m4 in matches_4.iter() {
                                        if adjacent(m3, m4, sentence) {
                                            result.push((m0.clone(), m1.clone(), m2.clone(), m3.clone(), m4.clone()))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}


pub struct Rule6<PA, PB, PC, PD, PE, PF, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>,
                        &RuleProductionArg<'a, PF::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
          PF: Pattern<StashValue>,
{
    sym: Sym,
    is_terminal: bool,
    pattern: (PA, PB, PC, PD, PE, PF),
    production: F,
    _phantom: SendSyncPhantomData<(V, StashValue)>,
}

impl<PA, PB, PC, PD, PE, PF, V, StashValue, F> Rule<StashValue> for Rule6<PA, PB, PC, PD, PE, PF, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>,
                        &RuleProductionArg<'a, PF::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
          PF: Pattern<StashValue>,
{
    fn apply(&self,
             stash: &Stash<StashValue>,
             sentence: &str)
             -> CoreResult<ParsedNodes<StashValue>> {
        let matches = self.matches(&stash, sentence)?;
        matches
            .iter()
            .filter_map(|sub| {
                let nodes = svec!(sub.0.to_node(), sub.1.to_node(), sub.2.to_node(), sub.3.to_node(), sub.4.to_node(), sub.5.to_node());
                if stash
                       .iter()
                       .all(|old_node| {
                                old_node.root_node.children != nodes ||
                                old_node.root_node.rule_sym != self.sym
                            }) {
                    let byte_range = Range(sub.0.byte_range().0, sub.5.byte_range().1);
                    match (self.production)(&RuleProductionArg::new(sentence, &sub.0),
                                            &RuleProductionArg::new(sentence, &sub.1),
                                            &RuleProductionArg::new(sentence, &sub.2),
                                            &RuleProductionArg::new(sentence, &sub.3),
                                            &RuleProductionArg::new(sentence, &sub.4),
                                            &RuleProductionArg::new(sentence, &sub.5)) {
                        Ok(v) => { 
                          let payload = v.extract_payload();
                          Some(Ok(ParsedNode::new(self.sym, v.clone().into(), byte_range, payload, nodes)))
                        },
                        Err(RuleError(RuleErrorKind::Invalid, _)) => None,
                        Err(e) => Some(Err(make_production_error(e))),
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_terminal(&self) -> bool {
        self.is_terminal
    }
}

impl<PA, PB, PC, PD, PE, PF, V, StashValue, F> Rule6<PA, PB, PC, PD, PE, PF, V, StashValue, F>
    where V: NodePayload,
          StashValue: NodePayload<Payload=V::Payload> + From<V>,
          F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                        &RuleProductionArg<'a, PB::M>,
                        &RuleProductionArg<'a, PC::M>,
                        &RuleProductionArg<'a, PD::M>,
                        &RuleProductionArg<'a, PE::M>,
                        &RuleProductionArg<'a, PF::M>)
                        -> RuleResult<V> + Send + Sync,
          PA: Pattern<StashValue>,
          PB: Pattern<StashValue>,
          PC: Pattern<StashValue>,
          PD: Pattern<StashValue>,
          PE: Pattern<StashValue>,
          PF: Pattern<StashValue>,
{
    pub fn new(sym: Sym, is_terminal: bool, pat: (PA, PB, PC, PD, PE, PF), prod: F) -> Rule6<PA, PB, PC, PD, PE, PF, V, StashValue, F> {
        Rule6 {
            sym: sym,
            is_terminal: is_terminal,
            pattern: pat,
            production: prod,
            _phantom: SendSyncPhantomData::new(),
        }
    }

    fn matches(&self,
               stash: &Stash<StashValue>,
               sentence: &str)
               -> CoreResult<PredicateMatches<(PA::M, PB::M, PC::M, PD::M, PE::M, PF::M)>> {
        let mut result = PredicateMatches::default();
        let matches_0 = self.pattern.0.predicate(stash, sentence)?;
        if matches_0.is_empty() {
            return Ok(result);
        }
        let matches_1 = self.pattern.1.predicate(stash, sentence)?;
        if matches_1.is_empty() {
            return Ok(result);
        }
        let matches_2 = self.pattern.2.predicate(stash, sentence)?;
        if matches_2.is_empty() {
            return Ok(result);
        }
        let matches_3 = self.pattern.3.predicate(stash, sentence)?;
        if matches_3.is_empty() {
            return Ok(result);
        }
        let matches_4 = self.pattern.4.predicate(stash, sentence)?;
        if matches_4.is_empty() {
            return Ok(result);
        }
        let matches_5 = self.pattern.5.predicate(stash, sentence)?;
        if matches_5.is_empty() {
            return Ok(result);
        }
        for m0 in matches_0.iter() {
            for m1 in matches_1.iter() {
                if adjacent(m0, m1, sentence) {
                    for m2 in matches_2.iter() {
                        if adjacent(m1, m2, sentence) {
                            for m3 in matches_3.iter() {
                                if adjacent(m2, m3, sentence) {
                                    for m4 in matches_4.iter() {
                                        if adjacent(m3, m4, sentence) {
                                            for m5 in matches_5.iter() {
                                              if adjacent(m4, m5, sentence) {
                                                  result.push((m0.clone(), m1.clone(), m2.clone(), m3.clone(), m4.clone(), m5.clone()))
                                              }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
#[allow(unused_mut)]
mod tests {
    use rule::*;
    use helpers::BoundariesChecker;

    macro_rules! svec {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::new();
            $( v.push($item); )*
            v
        }
        }
    }

    macro_rules! svec2 {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::<[_;2]>::new();
            $( v.push($item); )*
            v
        }
        }
    }

    macro_rules! svec4 {
        ($($item:expr),*) => { {
            let mut v = ::smallvec::SmallVec::<[_;4]>::new();
            $( v.push($item); )*
            v
        }
        }
    }

    impl AttemptFrom<usize> for usize {
        fn attempt_from(v: usize) -> Option<usize> {
            Some(v)
        }
    }

    impl NodePayload for usize {
        type Payload = usize;
        fn extract_payload(&self) -> Option<usize> {
            Some(*self)
        }
    }

    macro_rules! reg {
        ($st:expr, $typ:ty, $pattern:expr) => ( $crate::pattern::TextPattern::<$typ>::new(::regex::Regex::new($pattern).unwrap(), $st.sym($pattern), BoundariesChecker::SperatedAlphanumericWord) )
    }

    #[test]
    fn test_integer_numeric_en_rule() {
        let mut st = ::SymbolTable::default();
        let ten = st.sym("ten");
        let rule = Rule1::new(ten, (reg!(st, usize, "ten")), |_| Ok(10usize));
        assert_eq!(vec![Text::new(svec![Range(8, 11)], Range(8, 11), ten)],
                   rule.matches(&vec![], "foobar: ten").unwrap());
        assert_eq!(vec![Text::new(svec![Range(8, 11)], Range(8, 11), ten),
                        Text::new(svec![Range(12, 15)], Range(12, 15), ten)],
                   rule.matches(&vec![], "foobar: ten ten").unwrap());
        assert_eq!(svec4![ParsedNode::new(ten,
                                          10usize,
                                          Range(8, 11),
                                          Some(10usize),
                                          svec![Node::new(ten, Range(8, 11), None, svec![])]),
                          ParsedNode::new(ten,
                                          10usize,
                                          Range(12, 15),
                                          Some(10usize),
                                          svec![Node::new(ten, Range(12, 15), None, svec![])])],
                   rule.apply(&vec![], "foobar: ten ten").unwrap())
    }

    #[test]
    fn test_integer_numeric_compo_en_rule() {
        let mut st = ::SymbolTable::default();
        let rule_consec =
            Rule2::new(st.sym("2 consecutive ints"),
                       (AnyNodePattern::<usize>::new(),
                        FilterNodePattern::<usize>::filter(vec![Box::new(|integer: &usize| {
                                                                             *integer == 10
                                                                         })])),
                       |a, b| Ok(a.value() + b.value()));
        let stash: Stash<usize> = vec![ParsedNode::new(st.sym("ten"), 10, Range(8, 11), None, svec![]),
                                       ParsedNode::new(st.sym("ten"), 10, Range(12, 15), None, svec![])];
        assert_eq!(vec![(stash[0].clone(), stash[1].clone())],
                   rule_consec.matches(&stash, "foobar: ten ten").unwrap());
        assert_eq!(svec4![ParsedNode::new(st.sym("2 consecutive ints"),
                                          20,
                                          Range(8, 15),
                                          Some(20),
                                          svec![stash[0].root_node.clone(),
                                                stash[1].root_node.clone()])],
                   rule_consec.apply(&stash, "foobar: ten ten").unwrap());
    }

    #[test]
    fn test_integer_numeric_int_rule() {
        use std::str::FromStr;
        let mut st = ::SymbolTable::default();
        let rule_int = Rule1::new(st.sym("int"),
                                  (reg!(st, usize, "\\d+")),
                                  |a| Ok(usize::from_str(&*a.group(0))?));
        assert_eq!(svec4![ParsedNode::new(st.sym("int"),
                                          42,
                                          Range(8, 10),
                                          Some(42),
                                          svec![Node::new(st.sym("\\d+"),
                                                          Range(8, 10),
                                                          None,
                                                          svec![])])],
                   rule_int.apply(&vec![], "foobar: 42").unwrap());
    }

}

