use std::collections::HashMap;

use regex_syntax::{
    hir::{visit, Class, Hir, HirKind, Visitor},
    parse,
};

use crate::data::{
    lexer::LexerData,
    tokens::{TokenPattern, TokensData},
};

#[derive(Debug)]
pub enum UnsupportedFeature {
    EmptyPattern,
    LookaheadPattern,
    ByteCharClass,
    NonGreedyRepetition,
}

#[derive(Debug)]
pub enum NFACompileError {
    Message(String),
    Unsupported(UnsupportedFeature, Hir),
}

pub fn compile(regex: &str) -> Result<NFA, NFACompileError> {
    let hir = parse(regex).map_err(|e| NFACompileError::Message(format!("{e}")))?;
    visit_hir(&hir).map_err(|feat| NFACompileError::Unsupported(feat, hir))
}

#[derive(Debug, Clone)]
pub enum Transition {
    Epsilon,
    Char(char),
    Chars { start: char, end: char },
}

#[derive(Debug, Clone)]
pub enum StateKind {
    NotAccepting,
    Accepting,
}

#[derive(Debug, Clone)]
pub struct State {
    pub transitions: Vec<(Transition, usize)>,
    pub kind: StateKind,
}

impl State {
    fn add_transition(&mut self, transition: Transition, target: usize) {
        self.transitions.push((transition, target));
    }
}

#[derive(Clone)]
pub struct NFA {
    states: HashMap<usize, State>,
    end_state: usize,
}

impl NFA {
    #[inline]
    pub fn start_state(&self) -> usize {
        0
    }

    #[inline]
    pub fn end_state(&self) -> usize {
        self.end_state
    }

    #[inline]
    pub fn states(&self) -> &HashMap<usize, State> {
        &self.states
    }

    fn reindex_states(mut self, offset: usize) -> Self {
        self.end_state += offset;

        let start_state = self.start_state();

        self.states = self
            .states
            .into_iter()
            .map(|(mut id, mut state)| {
                if id != start_state {
                    id += offset;
                }

                for (_, next_state) in &mut state.transitions {
                    *next_state += offset;
                }

                (id, state)
            })
            .collect();

        self
    }
}

impl std::fmt::Debug for NFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state_ids: Vec<_> = self.states.keys().cloned().collect();
        state_ids.sort();

        for state_id in state_ids {
            if let Some(state) = self.states.get(&state_id) {
                write!(f, "State {} {:?}: ", state_id, state.kind)?;

                for (i, t) in state.transitions.iter().enumerate() {
                    write!(f, "{:?}", t)?;

                    if i < state.transitions.len() - 1 {
                        write!(f, ", ");
                    }
                }

                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
struct NFABuilder {
    states: HashMap<usize, State>,
    state_id_counter: usize,
}

impl NFABuilder {
    fn new() -> Self {
        let mut builder = NFABuilder {
            state_id_counter: 0,
            states: HashMap::new(),
        };

        let start_state = State {
            kind: StateKind::NotAccepting,
            transitions: Vec::new(),
        };
        builder.states.insert(builder.start_state(), start_state);

        builder
    }

    fn start_state(&self) -> usize {
        0
    }

    fn end_state(&self) -> usize {
        self.state_id_counter
    }

    fn create_state(&mut self, kind: StateKind) -> usize {
        self.state_id_counter += 1;
        let id = self.state_id_counter;

        let state = State {
            kind,
            transitions: Vec::new(),
        };
        self.states.insert(id, state);

        id
    }

    fn add_transition(&mut self, from: usize, to: usize, transition: Transition) {
        if let Some(state) = self.states.get_mut(&from) {
            state.add_transition(transition, to);
        }
    }

    fn append_nfa(&mut self, state_id: usize, mut nfa: NFA) {
        let offset = self.end_state();
        let mut reindexed_nfa = nfa.reindex_states(offset);
        let nfa_start = reindexed_nfa
            .states
            .remove(&reindexed_nfa.start_state())
            .expect("NFA start state not found");

        // FIX: what happens if state kinds differ for state_id and nfa_start

        self.states
            .entry(state_id)
            .and_modify(|state| state.transitions.extend(nfa_start.transitions));

        self.states.extend(reindexed_nfa.states);
        self.state_id_counter = reindexed_nfa.end_state;
    }

    fn build(self) -> NFA {
        NFA {
            end_state: self.end_state(),
            states: self.states,
        }
    }
}

fn visit_hir(hir: &Hir) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

    match hir.kind() {
        HirKind::Empty => return Err(UnsupportedFeature::EmptyPattern),
        HirKind::Look(look) => return Err(UnsupportedFeature::LookaheadPattern),
        HirKind::Literal(literal) => {
            let literal = String::from_utf8(literal.0.to_vec())
                // TODO: error message to say this is a bug
                .expect("invalid UTF-8 parsed by regex_syntax");

            let mut parent_id = builder.start_state();

            for (idx, c) in literal.chars().enumerate() {
                let kind = if idx == literal.len() - 1 {
                    StateKind::Accepting
                } else {
                    StateKind::NotAccepting
                };

                let id = builder.create_state(kind);
                builder.add_transition(parent_id, id, Transition::Char(c));
                parent_id = id;
            }
        }
        HirKind::Class(class) => {
            match class {
                Class::Unicode(class) => {
                    let state = builder.create_state(StateKind::Accepting);

                    for class in class.iter() {
                        let start = class.start();
                        let end = class.end();

                        builder.add_transition(
                            builder.start_state(),
                            state,
                            Transition::Chars { start, end },
                        );
                    }
                }
                Class::Bytes(class) => return Err(UnsupportedFeature::ByteCharClass),
            };
        }
        HirKind::Repetition(repetition) => {
            if !repetition.greedy {
                return Err(UnsupportedFeature::NonGreedyRepetition);
            }

            let nfa = visit_hir(&repetition.sub)?;

            let mut nfa_no_accepting = nfa.clone();
            nfa_no_accepting
                .states
                .iter_mut()
                .for_each(|(_, state)| state.kind = StateKind::NotAccepting);

            // first min-1 NFAs must not accept transformations
            for _ in 1..repetition.min {
                builder.append_nfa(builder.end_state(), nfa_no_accepting.clone());
            }

            builder.append_nfa(builder.end_state(), nfa.clone());

            if let Some(max) = repetition.max {
                let mut intermediary_states = vec![builder.end_state()];

                // unwind all other repetitions till max
                for _ in repetition.min..max {
                    builder.append_nfa(builder.end_state(), nfa.clone());
                    let state_after = builder.create_state(StateKind::Accepting);
                    builder.add_transition(state_after - 1, state_after, Transition::Epsilon);
                    intermediary_states.push(state_after);
                }

                // connect all intermediary states to the end state
                let mut end_state = builder.create_state(StateKind::Accepting);
                for intermediary_state in intermediary_states {
                    builder.add_transition(intermediary_state, end_state, Transition::Epsilon);
                }
            } else {
                let state_before = builder.end_state();
                builder.append_nfa(builder.end_state(), nfa);
                builder.add_transition(builder.end_state(), state_before, Transition::Epsilon);
                let state_after = builder.create_state(StateKind::Accepting);
                builder.add_transition(state_before, state_after, Transition::Epsilon);
            }
        }
        HirKind::Capture(capture) => return visit_hir(&capture.sub),
        HirKind::Concat(hirs) => {
            for hir in hirs {
                let nfa = visit_hir(hir)?;
                builder.append_nfa(builder.end_state(), nfa);
            }
        }
        HirKind::Alternation(hirs) => {
            let end_states = hirs
                .iter()
                .map(|hir| {
                    let nfa = visit_hir(hir)?;
                    builder.append_nfa(builder.start_state(), nfa);
                    Ok(builder.end_state())
                })
                .collect::<Result<Vec<_>, _>>()?;

            // reconnect dangling ending states of inner NFAs
            let mut end_state = builder.create_state(StateKind::Accepting);
            for end_state_ in end_states {
                builder.add_transition(end_state_, end_state, Transition::Epsilon);
            }
        }
    };

    Ok(builder.build())
}
