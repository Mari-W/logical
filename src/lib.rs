/// helpers for logic formulas
/// libs: peg
///
///
use std::collections::HashMap;

/// recursive definition of some propositional logical formula
#[derive(Clone)]
pub enum Formula {
    True,
    False,
    Var(String),
    Neg(Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    Impl(Box<Formula>, Box<Formula>),
    Eq(Box<Formula>, Box<Formula>),
}

/// formatting wrappers
pub struct Assignment(HashMap<String, bool>);

pub struct Truthtable(HashMap<Assignment, bool>);

pub struct Assignments(Vec<Assignment>);

pub struct PossibleAssignment(Option<Assignment>);

peg::parser! {
    pub grammar parser() for str {
        rule _ = quiet!{[' ' | '\n' | '\t' | '\r']*}

        rule ident() -> String
                = cs:$(quiet!{['a' ..= 'z' | 'A' ..= 'Z' | '_']['a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9' | '_' ]*}) { cs.into() }

        rule atom() -> Formula
            = "1" { Formula::True }
            / "⊤" { Formula::True }
            / "T" { Formula::True }
            / "0" { Formula::False }
            / "⊥" { Formula::False }
            / "F" { Formula::False }
            / x:ident() { Formula::Var(x) }

        rule op() -> Formula
            = "~" f:formula() { Formula::negate(f) }
            / "¬" f:formula() { Formula::negate(f) }
            / "¬" f:formula() { Formula::negate(f) }
            / "(" _ f1:formula() _ "&" _ f2:formula() _ ")" { Formula::and(f1, f2) }
            / "(" _ f1:formula() _ "∧" _ f2:formula() _ ")" { Formula::and(f1, f2) }
            / "(" _ f1:formula() _ "|" _ f2:formula() _ ")" { Formula::or(f1, f2) }
            / "(" _ f1:formula() _ "∨" _ f2:formula() _ ")" { Formula::or(f1, f2) }
            / "(" _ f1:formula() _ "->" _ f2:formula() _ ")" { Formula::implies(f1, f2) }
            / "(" _ f1:formula() _ "→" _ f2:formula() _ ")" { Formula::implies(f1, f2) }
            / "(" _ f1:formula() _ "<->" _ f2:formula() _ ")" { Formula::equivalence(f1, f2) }
            / "(" _ f1:formula() _ "⟷" _ f2:formula() _ ")" { Formula::equivalence(f1, f2) }

        pub rule formula() -> Formula
            = _ o:op() _ { o }
            / _ a:atom() _ { a }
    }
}

impl Formula {
    pub fn parse(s: &str) -> Result<Formula, String> {
        parser::formula(s).map_err(|e| format! {"{:#?}", e})
    }

    pub fn negate(f: Formula) -> Self {
        return Formula::Neg(Box::new(f));
    }

    pub fn variable(s: &str) -> Self {
        return Formula::Var(s.into());
    }

    pub fn and(l: Formula, r: Formula) -> Self {
        return Formula::And(Box::new(l), Box::new(r));
    }

    pub fn or(l: Formula, r: Formula) -> Self {
        return Formula::Or(Box::new(l), Box::new(r));
    }

    pub fn implies(l: Formula, r: Formula) -> Self {
        return Formula::Impl(Box::new(l), Box::new(r));
    }

    pub fn equivalence(l: Formula, r: Formula) -> Self {
        return Formula::Eq(Box::new(l), Box::new(r));
    }

    pub fn variables(&self) -> Vec<String> {
        let mut vars = self.variable_names();
        vars.sort();
        vars.dedup();
        vars
    }

    fn variable_names(&self) -> Vec<String> {
        match self {
            Formula::Var(s) => vec![s.clone()],
            Formula::Neg(f) => (&*f).variable_names(),
            Formula::And(l, r) |
            Formula::Or(l, r) |
            Formula::Impl(l, r) |
            Formula::Eq(l, r) => {
                [(&*r).variable_names(), (&*l).variable_names()].concat()
            }
            _ => vec![]
        }
    }

    pub fn is_satisfiable(&self) -> bool {
        self.truthtable().0.values().any(|b| *b)
    }

    pub fn is_tautology(&self) -> bool {
        !self.truthtable().0.values().any(|b| !(*b))
    }

    pub fn satisfying_assignments(&self) -> Assignments {
        Assignments(self.truthtable().0
            .into_iter()
            .filter(|(_, b)| *b)
            .map(|(a, _)| a)
            .collect()
        )
    }

    pub fn unsatisfying_assignments(&self) -> Assignments {
        Assignments(self.truthtable().0
            .into_iter()
            .filter(|(_, b)| !(*b))
            .map(|(a, _)| a)
            .collect()
        )
    }

    pub fn kdnf(&self) -> Formula {
        let mut kdnf = Formula::False;

        fn assignment_to_conjunction(a: Assignment) -> Formula {
            let mut f = Formula::False;

            let vars: Vec<_> = {
                let mut vs: Vec<_> = a.0.iter().collect();
                vs.sort_by_key(|a| a.0);
                vs.reverse();
                vs
            };

            for (x, v) in vars.into_iter() {
                match f {
                    Formula::False => {
                        f = if *v { Formula::Var(x.to_string()) } else { Formula::negate(Formula::Var(x.to_string())) }
                    }
                    Formula::Var(_) | Formula::And(_, _) | Formula::Neg(_) => {
                        f = if *v { Formula::and(f.clone(), Formula::Var(x.to_string())) } else { Formula::and(f.clone(), Formula::negate(Formula::Var(x.to_string()))) }
                    }
                    _ => panic!("_this_ should not have happened!")
                }
            }
            f
        }

        for (k, v) in self.truthtable().0 {
            if !v {
                continue;
            }
            match kdnf {
                Formula::False => {
                    kdnf = assignment_to_conjunction(k)
                }
                Formula::And(_, _) | Formula::Or(_, _) => {
                    kdnf = Formula::or(kdnf.clone(), assignment_to_conjunction(k))
                }
                _ => panic!("_this_ should not have happened!")
            }
        }
        kdnf
    }

    pub fn truthtable(&self) -> Truthtable {
        let variables = self.variables();
        let mut table = Truthtable(HashMap::new());
        for i in 0usize..2usize.pow(variables.len().try_into().unwrap()) {
            let bin: &Vec<bool> = &format!("{:0512b}", i)[512 - variables.len()..]
                .chars()
                .into_iter()
                .map(|c| c == '0')
                .collect();

            let mut assignment = Assignment(HashMap::new());
            for (i, var) in variables.iter().enumerate() {
                assignment.0.insert(var.clone(), bin[i]);
            }
            let res = self.evaluate(&assignment.0).unwrap();
            table.0.insert(assignment, res);
        }
        table
    }

    pub fn evaluate(&self, beta: &HashMap<String, bool>) -> Result<bool, String> {
        match self {
            Formula::True => Ok(true),
            Formula::False => Ok(false),
            Formula::Var(s) => {
                if beta.contains_key(s) {
                    Ok(beta[s])
                } else {
                    Err(format!("Found no assigment for {}", s))
                }
            }
            Formula::Neg(f) => Ok(!(&*f).evaluate(&beta)?),
            Formula::And(l, r) => Ok((&*l).evaluate(&beta)? && (&*r).evaluate(&beta)?),
            Formula::Or(l, r) => Ok((&*l).evaluate(&beta)? || (&*r).evaluate(&beta)?),
            Formula::Impl(l, r) => Ok(!(&*l).evaluate(&beta)? || (&*r).evaluate(&beta)?),
            Formula::Eq(l, r) => Ok(!((&*l).evaluate(&beta)? ^ (&*r).evaluate(&beta)?))
        }
    }
}

/// formatting
impl std::hash::Hash for Assignment {
    fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
        for (k, v) in &self.0 {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl PartialEq<Self> for Assignment {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Assignment {}


impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut assigments = self.0
            .iter()
            .map(|(k, v)| format!("{} ⇒ {}", k, if *v { "⊤" } else { "⊥" }))
            .collect::<Vec<String>>();
        assigments.sort();
        write!(f, "{}", assigments.join(", "))
    }
}

impl std::fmt::Display for PossibleAssignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.0 {
            Some(a) => {
                let mut assignments = a.0
                    .iter()
                    .map(|(k, v)| format!("{} ⇒ {}", k, if *v { "⊤" } else { "⊥" }))
                    .collect::<Vec<String>>();
                assignments.sort();
                write!(f, "{}", assignments.join(", "))
            }
            None => write!(f, "No assigment found.")
        }
    }
}

impl std::fmt::Display for Truthtable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut assignments: Vec<String> = self.0
            .iter()
            .map(|(a, v)| format!("({}) ⇒ {}", a, if *v { "⊤" } else { "⊥" }))
            .collect();
        assignments.sort();
        write!(f, "{}", assignments.join("\n"))
    }
}


impl std::fmt::Display for Assignments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut assignments: Vec<String> = self.0
            .iter()
            .map(|a| format!("{}", a))
            .collect();
        assignments.sort();
        write!(f, "{}", assignments.join("\n"))
    }
}


impl std::fmt::Display for Formula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            Formula::True => write!(f, "⊤"),
            Formula::False => write!(f, "⊥"),
            Formula::Var(s) => write!(f, "{}", s),
            Formula::Neg(fo) => write!(f, "¬{}", &*fo),
            Formula::And(l, r) => write!(f, "({} ∧ {})", &*r, &*l),
            Formula::Or(l, r) => write!(f, "({} ∨ {})", &*r, &*l),
            Formula::Impl(l, r) => write!(f, "({} → {})", &*r, &*l),
            Formula::Eq(l, r) => write!(f, "({} ⟷ {})", &*r, &*l)
        }
    }
}
