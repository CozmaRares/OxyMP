#[derive(Debug)]
struct Token;

trait State {
    fn transition(self, c: char) -> Result<StateEnum, char>;
    fn make_token(matched: &str) -> Token;
    // ??
}

struct S0;
struct S1;
struct S2;

impl State for S0 {
    fn transition(self, c: char) -> Result<StateEnum, char> {
        match c {
            'a' => Ok(StateEnum::S1(S1)),
            _ => Err(c),
        }
    }

    fn make_token(matched: &str) -> Token {
        Token
    }
}

impl State for S1 {
    fn transition(self, c: char) -> Result<StateEnum, char> {
        match c {
            'a' => Ok(StateEnum::S1(S1)),
            'b' => Ok(StateEnum::S2(S2)),
            _ => Err(c),
        }
    }

    fn make_token(matched: &str) -> Token {
        Token
    }
}

impl State for S2 {
    fn transition(self, c: char) -> Result<StateEnum, char> {
        match c {
            'a' => Ok(StateEnum::S1(S1)),
            _ => Err(c),
        }
    }

    fn make_token(matched: &str) -> Token {
        Token
    }
}

enum StateEnum {
    S0(S0),
    S1(S1),
    S2(S2),
}

impl StateEnum {
    fn transition(self, c: char) -> Result<StateEnum, char> {
        match self {
            StateEnum::S0(state) => state.transition(c),
            StateEnum::S1(state) => state.transition(c),
            StateEnum::S2(state) => state.transition(c),
        }
    }

    fn make_token(&self, matched: &str) -> Token {
        match self {
            StateEnum::S0(_) => S0::make_token(matched),
            StateEnum::S1(_) => S1::make_token(matched),
            StateEnum::S2(_) => S2::make_token(matched),
        }
    }
}

struct DFA;

impl DFA {
    // return string and state
    fn process_string(input: &str) -> Result<Token, String> {
        let mut state = StateEnum::S0(S0);

        for c in input.chars() {
            state = match state.transition(c) {
                Ok(state) => state,
                Err(c) => return Err(format!("Unexpected character '{}'", c)),
            };
        }

        Ok(state.make_token(input))
    }
}

pub fn example() {
    println!(
        "'ab' accepted: {:?} (expected true)",
        DFA::process_string("ab")
    );
    println!(
        "'abb' accepted: {:?} (expected false)",
        DFA::process_string("abb")
    );
    println!(
        "'aab' accepted: {:?} (expected true)",
        DFA::process_string("aab")
    );
}
