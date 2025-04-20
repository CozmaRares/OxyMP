use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
};

// TODO: consistend naming: state_id for usize and state for State

use regex_syntax::{
    hir::{Class, ClassUnicode, Hir, HirKind, Literal, Repetition},
    parse,
};

use crate::{
    data::{
        lexer::LexerData,
        tokens::{TokenPattern, TokensData},
    },
    utils::capitalize,
};

#[derive(Debug, thiserror::Error)]
pub enum UnsupportedFeature {
    #[error("empty pattern")]
    EmptyPattern,
    #[error("lookahead pattern")]
    LookaheadPattern,
    #[error("byte char class")]
    ByteCharClass,
}

#[derive(Debug, thiserror::Error)]
pub enum NFACompileError {
    #[error("{}", capitalize(format!("{}", .0)))]
    Message(#[from] regex_syntax::Error),
    #[error("NFA compilation encountered an unsupported regex feature: {0}")]
    Unsupported(#[from] UnsupportedFeature),
    #[error("NFA compiled from '{0}' allows matching an empty string. Token variants must match at least one character.")]
    PatternMatchesEmptyString(String),
}

type NFACompileResult = Result<NFA, NFACompileError>;

pub fn compile<'a>(regex: &str, tag: StateTag) -> NFACompileResult {
    let hir = parse(regex)?;
    let mut nfa = visit_hir(&hir)?;

    if nfa.matches_empty_string() {
        return Err(NFACompileError::PatternMatchesEmptyString(
            regex.to_string(),
        ));
    };

    nfa.set_states_tag(tag);
    Ok(nfa)
}

// handled exactly like an alteration
pub fn combine(nfas: Vec<NFA>) -> NFA {
    let mut builder = NFABuilder::new();

    let end_states: Vec<_> = nfas
        .into_iter()
        .map(|nfa| {
            builder.append_nfa(builder.start_state(), nfa);
            builder.end_state()
        })
        .collect();

    // reconnect dangling ending states of inner NFAs
    let mut end_state = builder.create_state(StateKind::Accepting);
    for end_state_ in end_states {
        builder.add_transition(end_state_, end_state, Transition::Epsilon);
    }

    builder.build()
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
pub enum StateTag {
    Skip,
    Token(String),
    None,
}

#[derive(Debug, Clone)]
pub struct State {
    pub transitions: Vec<(Transition, usize)>,
    pub kind: StateKind,
    pub tag: StateTag,
}

impl State {
    fn new() -> Self {
        Self {
            transitions: Vec::new(),
            kind: StateKind::NotAccepting,
            tag: StateTag::None,
        }
    }

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
    fn set_states_tag(&mut self, tag: StateTag) {
        self.states.iter_mut().for_each(|(id, state)| {
            if matches!(state.kind, StateKind::Accepting) {
                state.tag = tag.clone();
            }
        });
    }
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

    fn to_not_accepting(mut self) -> Self {
        self.states
            .iter_mut()
            .for_each(|(_, state)| state.kind = StateKind::NotAccepting);
        self
    }

    fn assert_valid(&self) {
        assert!(
            self.states.contains_key(&self.start_state()),
            "NFA start state not found"
        );
        assert!(
            self.states.contains_key(&self.end_state()),
            "NFA end state not found"
        );

        assert!(
            self.states
                .get(&self.start_state())
                .unwrap()
                .transitions
                .len()
                != 0,
            "NFA start state must have at least one transition"
        );
        assert!(
            self.states
                .get(&self.end_state())
                .unwrap()
                .transitions
                .len()
                == 0,
            "NFA end state can't have any transitions"
        );

        for (state_id, state) in &self.states {
            for (_, next_state) in &state.transitions {
                assert!(
                    self.states.contains_key(next_state),
                    "Transition target state {} not found",
                    next_state
                );

                assert!(
                    *next_state != self.start_state(),
                    "State {} can't have a transition to the start state",
                    state_id
                );
            }
        }
    }

    fn matches_empty_string(&self) -> bool {
        let mut queue = VecDeque::new();
        queue.push_back(self.start_state());

        while let Some(state_id) = queue.pop_front() {
            if state_id == self.end_state() {
                return true;
            }

            let Some(state) = self.states.get(&state_id) else {
                continue;
            };

            for (transition, next_state_id) in &state.transitions {
                match transition {
                    Transition::Epsilon => queue.push_back(*next_state_id),
                    _ => {}
                }
            }
        }

        false
    }
}

impl Debug for NFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state_ids: Vec<_> = self.states.keys().cloned().collect();
        state_ids.sort();

        for state_id in state_ids {
            if let Some(state) = self.states.get(&state_id) {
                write!(f, "State ({}:{:?}) {:?}: ", state_id, state.tag, state.kind)?;

                for (i, t) in state.transitions.iter().enumerate() {
                    write!(f, "{:?}", t)?;

                    if i < state.transitions.len() - 1 {
                        write!(f, ", ")?;
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

        let start_state = State::new();
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

        let mut state = State::new();
        state.kind = kind;

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

        self.states
            .entry(state_id)
            .and_modify(|state| state.transitions.extend(nfa_start.transitions));

        self.states.extend(reindexed_nfa.states);
        self.state_id_counter = reindexed_nfa.end_state;
    }

    fn build(self) -> NFA {
        let nfa = NFA {
            end_state: self.end_state(),
            states: self.states,
        };
        nfa.assert_valid();
        nfa
    }
}

fn visit_hir(hir: &Hir) -> Result<NFA, UnsupportedFeature> {
    match hir.kind() {
        HirKind::Empty => Err(UnsupportedFeature::EmptyPattern),
        HirKind::Look(look) => Err(UnsupportedFeature::LookaheadPattern),
        HirKind::Literal(literal) => Ok(visit_literal(literal)),
        HirKind::Class(class) => match class {
            Class::Unicode(class) => Ok(visit_class(class)),
            Class::Bytes(class) => Err(UnsupportedFeature::ByteCharClass),
        },
        HirKind::Repetition(repetition) => visit_repetition(repetition),
        HirKind::Capture(capture) => visit_hir(&capture.sub),
        HirKind::Concat(hirs) => visit_concat(hirs),
        HirKind::Alternation(hirs) => visit_alternation(hirs),
    }
}

fn visit_literal(literal: &Literal) -> NFA {
    let literal = String::from_utf8(literal.0.to_vec())
        // TODO: error message to say this is a bug
        .expect("invalid UTF-8 parsed by regex_syntax");

    let mut builder = NFABuilder::new();

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

    builder.build()
}

fn visit_class(class: &ClassUnicode) -> NFA {
    let mut builder = NFABuilder::new();

    let state = builder.create_state(StateKind::Accepting);

    for class in class.iter() {
        let start = class.start();
        let end = class.end();

        builder.add_transition(
            builder.start_state(),
            state,
            if start == end {
                Transition::Char(start)
            } else {
                Transition::Chars { start, end }
            },
        );
    }

    builder.build()
}

fn visit_repetition(repetition: &Repetition) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

    let nfa = visit_hir(&repetition.sub)?;

    if repetition.min > 0 {
        let nfa_not_accepting = nfa.clone().to_not_accepting();

        // first min-1 NFAs must not accept transformations
        for _ in 1..repetition.min {
            builder.append_nfa(builder.end_state(), nfa_not_accepting.clone());
        }

        // last NFA to meet the min requirement
        builder.append_nfa(builder.end_state(), nfa.clone());
    }

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
        let end_state = builder.end_state();
        for intermediary_state in intermediary_states {
            if intermediary_state != end_state {
                builder.add_transition(intermediary_state, end_state, Transition::Epsilon);
            }
        }
    } else {
        let state_before_loop = builder.create_state(StateKind::Accepting);
        builder.add_transition(
            state_before_loop - 1,
            state_before_loop,
            Transition::Epsilon,
        );
        builder.append_nfa(state_before_loop, nfa);
        builder.add_transition(builder.end_state(), state_before_loop, Transition::Epsilon);

        let end_state = builder.create_state(StateKind::Accepting);
        builder.add_transition(state_before_loop, end_state, Transition::Epsilon);
    }

    if !repetition.greedy {
        // if repetition is not greedy, we can jump dirrectly to the end state
        builder.add_transition(
            builder.start_state(),
            builder.end_state(),
            Transition::Epsilon,
        );
    }

    Ok(builder.build())
}

fn visit_concat(hirs: &[Hir]) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

    for (idx, hir) in hirs.iter().enumerate() {
        let mut nfa = visit_hir(hir)?;

        if idx != hirs.len() - 1 {
            nfa = nfa.to_not_accepting();
        }

        builder.append_nfa(builder.end_state(), nfa);
    }

    Ok(builder.build())
}

fn visit_alternation(hirs: &[Hir]) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

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

    Ok(builder.build())
}
