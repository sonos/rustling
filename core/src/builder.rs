use cell;

use {CoreResult, Pattern, RuleSet, Sym, SymbolTable};
use pattern;
use rule::{Rule, Rule1, Rule2, Rule3, RuleProductionArg};

use rule::rule_errors::*;

pub struct RuleSetBuilder<StashValue: Clone + Send + Sync> {
    symbols: cell::RefCell<SymbolTable>,
    rules: cell::RefCell<Vec<Box<Rule<StashValue>>>>,
}

impl<StashValue: Clone + Send + Sync> Default for RuleSetBuilder<StashValue> {
    fn default() -> RuleSetBuilder<StashValue> {
        RuleSetBuilder {
            symbols: cell::RefCell::new(SymbolTable::default()),
            rules: cell::RefCell::new(vec![]),
        }
    }
}

impl<StashValue: Clone + Send + Sync> RuleSetBuilder<StashValue> {

    pub fn sym<S>(&self, val: S) -> Sym
        where S: Into<String> + AsRef<str> {
            self.symbols.borrow_mut().sym(val)
        }

    pub fn rule_1<S, PA, V, F>(&self, sym: S, pa: PA, production: F)
        where S: Into<String> + AsRef<str>,
              V: Clone + Send + Sync + 'static,
              StashValue: From<V> + Clone + Send + Sync + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>) -> RuleResult<V> + Send + Sync + 'static,
              PA: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule1::new(sym, pa, production)))
    }

    pub fn rule_2<S, PA, PB, V, F>(&self, sym: S, pa: PA, pb: PB, production: F)
        where S: Into<String> + AsRef<str>,
              V: Clone + Send + Sync + 'static,
              StashValue: From<V> + Clone + Send + Sync + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>, &RuleProductionArg<'a, PB::M>)
                            -> RuleResult<V> + Send + Sync + 'static,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule2::new(sym, (pa, pb), production)))
    }

    pub fn rule_3<S, PA, PB, PC, V, F>(&self, sym: S, pa: PA, pb: PB, pc: PC, production: F)
        where S: Into<String> + AsRef<str>,
              V: Clone + Send + Sync + 'static,
              StashValue: From<V> + Clone + Send + Sync + 'static,
              F: for<'a> Fn(&RuleProductionArg<'a, PA::M>,
                            &RuleProductionArg<'a, PB::M>,
                            &RuleProductionArg<'a, PC::M>)
                            -> RuleResult<V> + Send + Sync + 'static,
              PA: Pattern<StashValue> + 'static,
              PB: Pattern<StashValue> + 'static,
              PC: Pattern<StashValue> + 'static
    {
        let sym = self.sym(sym);
        self.rules
            .borrow_mut()
            .push(Box::new(Rule3::new(sym, (pa, pb, pc), production)))
    }

    pub fn reg(&self, regex:&str) -> CoreResult<pattern::TextPattern<StashValue>> {
        Ok(pattern::TextPattern::new(::regex::Regex::new(regex)?, self.sym(regex)))
    }

    pub fn reg_neg_lh(&self, regex:&str, neg_lh:&str) -> CoreResult<pattern::TextNegLHPattern<StashValue>> {
        Ok(pattern::TextNegLHPattern::new(
                self.reg(regex)?,
                ::regex::Regex::new(neg_lh)?,
                self.sym(format!("{}(?:{})", regex, neg_lh))))
    }

    pub fn build(self) -> RuleSet<StashValue> {
        RuleSet { rules: self.rules.into_inner(), symbols: self.symbols.into_inner() }
    }
}
