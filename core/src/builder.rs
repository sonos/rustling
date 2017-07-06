use cell;

use {CoreResult, Pattern, TerminalPattern, RuleSet, Sym, SymbolTable, NodePayload};
use pattern;
use helpers::BoundariesChecker;
use rule::{Rule, Rule1, Rule2, Rule3, Rule4, Rule5, Rule6, RuleProductionArg};

use rule::rule_errors::*;

pub struct RuleSetBuilder<StashValue: NodePayload> {
    symbols: cell::RefCell<SymbolTable>,
    rules: cell::RefCell<Vec<Box<Rule<StashValue>>>>,
    boundaries_checker: BoundariesChecker,
}

impl<StashValue: NodePayload> RuleSetBuilder<StashValue> {
    pub fn new(boundaries_checker: BoundariesChecker) -> RuleSetBuilder<StashValue> {
        RuleSetBuilder {
            symbols: cell::RefCell::new(SymbolTable::default()),
            rules: cell::RefCell::new(vec![]),
            boundaries_checker,
        }
    }
}

impl<StashValue: NodePayload> RuleSetBuilder<StashValue> {

    pub fn sym<S>(&self, val: S) -> Sym
        where S: Into<String> + AsRef<str> {
            self.symbols.borrow_mut().sym(val)
        }

    
    pub fn rule_1<S, PA, V, F>(&self, sym: S, pa: PA, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule1::new(sym, false, pa, production)))
    }

    pub fn rule_1_terminal<S, PA, V, F>(&self, sym: S, pa: PA, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule1::new(sym, true, pa, production)))
    }
    
    pub fn rule_2<S, PA, PB, V, F>(&self, sym: S, pa: PA, pb: PB, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule2::new(sym, false, (pa, pb), production)))
    }

    pub fn rule_2_terminal<S, PA, PB, V, F>(&self, sym: S, pa: PA, pb: PB, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: TerminalPattern<StashValue> + 'static,
              PB: TerminalPattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule2::new(sym, true, (pa, pb), production)))
    }

    pub fn rule_3<S, PA, PB, PC, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>)
                            -> RuleResult<V> + 'static + Send + Sync,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule3::new(sym, false, (pa, pb, pc), production)))
    }

    
    pub fn rule_4<S, PA, PB, PC, PD, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
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
        self.rules
            .borrow_mut()
            .push(Box::new(Rule4::new(sym, false, (pa, pb, pc, pd), production)))
    }

    
    pub fn rule_5<S, PA, PB, PC, PD, PE, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
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
        self.rules
            .borrow_mut()
            .push(Box::new(Rule5::new(sym, false, (pa, pb, pc, pd, pe), production)))
    }

    
    pub fn rule_6<S, PA, PB, PC, PD, PE, PF, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, pd: PD, pe: PE, pf: PF, production: F)
        where S: Into<String> + AsRef<str>,
              V: NodePayload<Payload=StashValue::Payload> + 'static,
              StashValue: From<V> + 'static,
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
        self.rules
            .borrow_mut()
            .push(Box::new(Rule6::new(sym, false, (pa, pb, pc, pd, pe, pf), production)))
    }
    
    pub fn reg(&self, regex:&str) -> CoreResult<pattern::TextPattern<StashValue>> {
        Ok(pattern::TextPattern::new(::regex::Regex::new(regex)?, self.sym(regex), self.boundaries_checker))
    }

    pub fn reg_neg_lh(&self, regex:&str, neg_lh:&str) -> CoreResult<pattern::TextNegLHPattern<StashValue>> {
        Ok(pattern::TextNegLHPattern::new(
                ::regex::Regex::new(regex)?,
                ::regex::Regex::new(neg_lh)?,
                self.sym(format!("{}(?:{})", regex, neg_lh)),
                self.boundaries_checker))
    }

    pub fn build(self) -> RuleSet<StashValue> {
        RuleSet { rules: self.rules.into_inner(), symbols: self.symbols.into_inner() }
    }
}
