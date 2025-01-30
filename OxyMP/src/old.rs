use oxymp::RecursiveDescent;
use oxymp_util::lexer::{DefaultTokenTier, LexError, LexResult};

pub fn match_number(matched: &str) -> LexResult<i64> {
    matched.parse().map_err(|_| LexError::unparsable(matched))
}

pub fn match_ident(matched: &str) -> LexResult<String> {
    if matched.len() == 1 {
        Ok(matched.to_string())
    } else {
        Err(LexError::unparsable(matched))
    }
}

#[derive(RecursiveDescent)]
#[exact_pattern(name = "While", pattern = "while", tier = DefaultTokenTier::Low)]
#[exact_pattern(name = "ParanLeft", pattern = "(")]
#[exact_pattern(name = "ParanRight", pattern = ")")]
#[exact_pattern(name = "If", pattern = "if")]
#[exact_pattern(name = "Else", pattern = "else")]
#[regex_pattern(
    name = "Identifier",
    regex = "[a-z]",
    transformer_fn = match_ident,
    kind = String,
)]
#[exact_pattern(name = "Equal", pattern = "=")]
#[exact_pattern(name = "Plus", pattern = "+")]
#[exact_pattern(name = "Let", pattern = "let")]
#[regex_pattern(
    name = "Number",
    regex = r"[0-9]+",
    transformer_fn = match_number,
    kind = i64
)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = "EWh ::= While '(' E ')' E"]
#[grammar = "EIf ::= If '(' E ')' E Else E"]
#[grammar = "EEq ::= Identifier '=' E"]
#[grammar = "EPl1 ::= '+' T EPl1?"]
#[grammar = "EPl ::= T EPl1?"]
#[grammar = "E ::= EWh | EIf | EEq | EPl"]
#[grammar = "T ::= Number | Identifier | '(' E ')'"]
struct Parser;

macro_rules! instr {
    ($instr:literal) => {
        println!("    {}", $instr)
    };
    ($instr:literal, $arg:expr) => {
        println!("    {} {}", $instr, $arg)
    };
}

macro_rules! label {
    ($label:literal, $idx:expr) => {
        format!("{}{}:", $label, $idx)
    };
}

impl EWh {
    fn visit(&self, label_index: &mut usize) {
        let while_label = label!("_while", label_index);
        let after_label = label!("_after", label_index);
        *label_index += 1;

        println!("{}", while_label);
        self.0 .2.visit(label_index);
        instr!("jz", after_label);
        self.0 .4.visit(label_index);
        instr!("jmp", while_label);
        println!("{}", after_label);
    }
}

impl EIf {
    fn visit(&self, label_index: &mut usize) {
        let else_label = label!("_else", label_index);
        let after_label = label!("_after", label_index);

        self.0 .2.visit(label_index);
        instr!("jz", else_label);
        self.0 .4.visit(label_index);
        instr!("jmp", after_label);
        println!("{}", else_label);
        self.0 .6.visit(label_index);
        println!("{}", after_label);
    }
}

impl EEq {
    fn visit(&self, label_index: &mut usize) {
        self.0 .2.visit(label_index);
        let TokenIdentifier(var) = &self.0 .0;
        instr!("pop", var);
    }
}

impl EPl1 {
    fn visit(&self, label_index: &mut usize) {
        self.0 .1.visit(label_index);
        instr!("add");
        if let Some(expr) = &self.0 .2 {
            expr.visit(label_index);
        }
    }
}

impl EPl {
    fn visit(&self, label_index: &mut usize) {
        self.0 .0.visit(label_index);
        if let Some(expr) = &self.0 .1 {
            expr.visit(label_index);
        }
    }
}

impl E {
    fn visit(&self, label_index: &mut usize) {
        match &self.0 {
            EChoice1::_1(ewh) => ewh.visit(label_index),
            EChoice1::_2(eif) => eif.visit(label_index),
            EChoice1::_3(eeq) => eeq.visit(label_index),
            EChoice1::_4(epl) => epl.visit(label_index),
        }
    }
}

impl T {
    fn visit(&self, label_index: &mut usize) {
        match &self.0 {
            TChoice1::_1(TokenNumber(n)) => instr!("push", *n),
            TChoice1::_2(TokenIdentifier(s)) => instr!("push", s),
            TChoice1::_3((_, e, _)) => e.visit(label_index),
        }
    }
}

pub fn run() {
    let mut labels = 0;

    let l = create_lexer();

    let a = l.tokenize("x = 1 + 2 + 3").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit(&mut labels);

    let a = l.tokenize("x + 1").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit(&mut labels);

    let a = l.tokenize("while (1) x").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit(&mut labels);

    println!("{:#?}", a)
}
