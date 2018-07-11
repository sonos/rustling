use cell;

use {CoreResult, Pattern, StashIndexable, TerminalPattern, RuleSet, Sym, SymbolTable, NodePayload};
use pattern;
use helpers::BoundariesChecker;
use rule::{Rule, TerminalRule, Rule1, Rule2, Rule3, Rule4, Rule5, Rule6, RuleProductionArg};

use rule::RuleResult;

pub struct RuleSetBuilder<StashValue: NodePayload+StashIndexable> {
    symbols: cell::RefCell<SymbolTable>,
    composition_rules: cell::RefCell<Vec<Box<Rule<StashValue>>>>,
    terminal_rules: cell::RefCell<Vec<Box<TerminalRule<StashValue>>>>,
    word_boundaries: BoundariesChecker,
    match_boundaries: BoundariesChecker,
}

impl<StashValue: NodePayload+StashIndexable> RuleSetBuilder<StashValue> {
    pub fn new(word_boundaries: BoundariesChecker, match_boundaries: BoundariesChecker) -> RuleSetBuilder<StashValue> {
        RuleSetBuilder {
            symbols: cell::RefCell::new(SymbolTable::default()),
            composition_rules: cell::RefCell::new(vec![]),
            terminal_rules: cell::RefCell::new(vec![]),
            word_boundaries,
            match_boundaries,
        }
    }
}

impl<StashValue: NodePayload+StashIndexable> RuleSetBuilder<StashValue> {

    pub fn sym<S>(&self, val: S) -> Sym
        where S: Into<String> + AsRef<str> {
            self.symbols.borrow_mut().sym(val)
        }

    
    pub fn rule_1<S, PA, V, F>(&self, sym: S, pa: PA, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule1::new(sym, pa, production)))
    }

    pub fn rule_1_terminal<S, PA, V, F>(&self, sym: S, pa: PA, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.terminal_rules
            .borrow_mut()
            .push(Box::new(Rule1::new(sym, pa, production)))
    }
    
    pub fn rule_2<S, PA, PB, V, F>(&self, sym: S, pa: PA, pb: PB, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule2::new(sym, (pa, pb), production)))
    }

    pub fn rule_2_terminal<S, PA, PB, V, F>(&self, sym: S, pa: PA, pb: PB, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.terminal_rules
            .borrow_mut()
            .push(Box::new(Rule2::new(sym, (pa, pb), production)))
    }

    pub fn rule_3<S, PA, PB, PC, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule3::new(sym, (pa, pb, pc), production)))
    }

    pub fn rule_3_terminal<S, PA, PB, PC, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static,
              PC: TerminalPattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule3::new(sym, (pa, pb, pc), production)))
    }

    pub fn rule_4<S, PA, PB, PC, PD, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static,
              PD: Pattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule4::new(sym, (pa, pb, pc, pd), production)))
    }

    pub fn rule_4_terminal<S, PA, PB, PC, PD, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static,
              PC: TerminalPattern<StashValue> + 'static,
              PD: TerminalPattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule4::new(sym, (pa, pb, pc, pd), production)))
    }

    pub fn rule_5<S, PA, PB, PC, PD, PE, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>,
                            &RuleProductionArg<'a, PE::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static,
              PD: Pattern<StashValue> + 'static,
              PE: Pattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule5::new(sym, (pa, pb, pc, pd, pe), production)))
    }

    pub fn rule_5_terminal<S, PA, PB, PC, PD, PE, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>,
                            &RuleProductionArg<'a, PE::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static,
              PC: TerminalPattern<StashValue> + 'static,
              PD: TerminalPattern<StashValue> + 'static,
              PE: TerminalPattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule5::new(sym, (pa, pb, pc, pd, pe), production)))
    }
    
    pub fn rule_6<S, PA, PB, PC, PD, PE, PF, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, pf: PF, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>,
                            &RuleProductionArg<'a, PE::M>,
                            &RuleProductionArg<'a, PF::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static,
              PD: Pattern<StashValue> + 'static,
              PE: Pattern<StashValue> + 'static,
              PF: Pattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule6::new(sym, (pa, pb, pc, pd, pe, pf), production)))
    }

    pub fn rule_6_terminal<S, PA, PB, PC, PD, PE, PF, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, pf: PF, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: StashIndexable + From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>,
                            &RuleProductionArg<'a, PD::M>,
                            &RuleProductionArg<'a, PE::M>,
                            &RuleProductionArg<'a, PF::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static,
              PC: TerminalPattern<StashValue> + 'static,
              PD: TerminalPattern<StashValue> + 'static,
              PE: TerminalPattern<StashValue> + 'static,
              PF: TerminalPattern<StashValue> + 'static,
    {
        let sym = self.sym(sym);
        self.composition_rules
            .borrow_mut()
            .push(Box::new(Rule6::new(sym, (pa, pb, pc, pd, pe, pf), production)))
    }
    
    pub fn reg(&self, regex:&str) -> CoreResult<pattern::TextPattern<StashValue>> {
        Ok(pattern::TextPattern::new(::regex::Regex::new(regex)?, self.sym(regex), self.word_boundaries.clone()))
    }

    pub fn reg_neg_lh(&self, regex:&str, neg_lh:&str) -> CoreResult<pattern::TextNegLHPattern<StashValue>> {
        Ok(pattern::TextNegLHPattern::new(
                ::regex::Regex::new(regex)?,
                ::regex::Regex::new(neg_lh)?,
                self.sym(format!("{}(?:{})", regex, neg_lh)),
                self.word_boundaries.clone()))
    }

    pub fn build(self) -> RuleSet<StashValue> {
        RuleSet { symbols: self.symbols.into_inner(), terminal_rules: self.terminal_rules.into_inner(), composition_rules: self.composition_rules.into_inner(), match_boundaries: self.match_boundaries }
    }
}
