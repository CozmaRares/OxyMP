use std::collections::{HashMap, HashSet};

use regex_syntax::{
    hir::{Class, ClassUnicode, Hir, HirKind, Literal, Repetition},
    parse,
};

use crate::{range::Range, utils::capitalize};

#[derive(Debug, thiserror::Error)]
pub enum UnsupportedFeature {
    #[error("empty pattern (e.g., `\"\"`)")]
    EmptyPattern,
    #[error("lookahead pattern (e.g., `^abc`, `abc$`)")]
    LookaheadPattern,
    #[error("byte char class (e.g., `(?i-u)a`)")]
    ByteCharClass,
}

#[derive(Debug, thiserror::Error)]
pub enum NFACompileError {
    #[error("{}", capitalize(format!("{}", .0)))]
    Message(#[from] Box<regex_syntax::Error>),
    #[error("Regex compilation encountered an unsupported regex feature: {0}")]
    Unsupported(#[from] UnsupportedFeature),
    #[error("Pattern '{0}' allows matching an empty string. The pattern must always match at least one character.")]
    PatternMatchesEmptyString(String),
}

#[derive(Debug, Clone)]
pub enum Transition {
    Epsilon,
    Range(Range),
}

#[derive(Debug, Clone)]
pub enum StateKind {
    NotAccepting,
    Accepting,
    Inherit,
}

#[derive(Debug, Clone)]
pub enum StateTag {
    // pattern is used for testing
    #[cfg(debug_assertions)]
    Skip {
        pattern: String,
    },
    #[cfg(not(debug_assertions))]
    Skip,
    Token {
        variant: String,
        priority: usize,
    },
    None,
}

type StateId = usize;

#[derive(Debug, Clone)]
pub struct State {
    pub transitions: Vec<(Transition, StateId)>,
    pub kind: StateKind,
    pub tag: StateTag,
}

#[derive(Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct NFA {
    states: HashMap<StateId, State>,
    end_state_id: StateId,
}

pub fn compile(regex: &str, tag: StateTag) -> NFACompileResult {
    let hir = parse(regex).map_err(Box::new)?;
    let mut nfa = visit_hir(&hir)?;

    if nfa
        .compute_epsilon_closure(vec![nfa.start_state_id()])
        .contains(&nfa.end_state_id())
    {
        return Err(NFACompileError::PatternMatchesEmptyString(
            regex.to_string(),
        ));
    };

    nfa.tag_accepting_states(tag);
    Ok(nfa)
}

// handled exactly like an alteration
pub fn combine(nfas: Vec<NFA>) -> NFA {
    let mut builder = NFABuilder::new();

    let dangling_state_ids: Vec<_> = nfas
        .into_iter()
        .map(|nfa| {
            builder.append_nfa(builder.start_state_id(), nfa);
            builder.end_state_id()
        })
        .collect();

    // reconnect dangling ending states of inner NFAs
    let end_state_id = builder.create_state(StateKind::Inherit);
    for dangling_state_id in dangling_state_ids {
        builder.add_transition(dangling_state_id, end_state_id, Transition::Epsilon);
    }

    builder.build()
}

type NFACompileResult = Result<NFA, NFACompileError>;

impl State {
    fn new(kind: StateKind) -> Self {
        Self {
            transitions: Vec::new(),
            kind,
            tag: StateTag::None,
        }
    }

    fn add_transition(&mut self, transition: Transition, target_id: StateId) {
        self.transitions.push((transition, target_id));
    }
}

impl NFA {
    fn tag_accepting_states(&mut self, tag: StateTag) {
        self.states.iter_mut().for_each(|(_, state)| {
            if matches!(state.kind, StateKind::Accepting) {
                state.tag = tag.clone();
            }
        });
    }
}

impl NFA {
    #[inline]
    pub fn start_state_id(&self) -> StateId {
        0
    }

    #[inline]
    pub fn end_state_id(&self) -> StateId {
        self.end_state_id
    }

    #[inline]
    pub fn states(&self) -> &HashMap<StateId, State> {
        &self.states
    }

    fn reindex_states(mut self, offset: usize) -> Self {
        self.end_state_id += offset;

        let start_state_id = self.start_state_id();

        self.states = self
            .states
            .into_iter()
            .map(|(mut id, mut state)| {
                // don't change the start state's id
                if id != start_state_id {
                    id += offset;
                }

                state.transitions.iter_mut().for_each(|(_, state_id)| {
                    *state_id += offset;
                });

                (id, state)
            })
            .collect();

        self
    }

    fn convert_states_to_not_accepting(&mut self) {
        self.states
            .iter_mut()
            .for_each(|(_, state)| state.kind = StateKind::NotAccepting);
    }

    fn assert_valid(&self) {
        oxymp_assert!(
            self.states.contains_key(&self.start_state_id()),
            "NFA start state not found"
        );
        oxymp_assert!(
            self.states.contains_key(&self.end_state_id()),
            "NFA end state not found"
        );

        oxymp_assert!(
            !self
                .states
                .get(&self.start_state_id())
                .unwrap()
                .transitions
                .is_empty(),
            "NFA start state must have at least one transition"
        );
        oxymp_assert!(
            self.states
                .get(&self.end_state_id())
                .unwrap()
                .transitions
                .is_empty(),
            "NFA end state can't have any transitions"
        );

        for (state_id, state) in &self.states {
            match state.kind {
                StateKind::Accepting => {}
                _ => {
                    oxymp_assert!(
                        matches!(state.tag, StateTag::None),
                        "NFA state {} is not accepting but has a tag",
                        state_id
                    );
                }
            }

            for (transition, next_state_id) in &state.transitions {
                oxymp_assert!(
                    self.states.contains_key(next_state_id),
                    "Transition target state {} not found",
                    next_state_id
                );

                oxymp_assert!(
                    *next_state_id != self.start_state_id(),
                    "State {} can't have a transition to the start state",
                    state_id
                );

                if let Transition::Epsilon = transition {
                    oxymp_assert!(
                        next_state_id != state_id,
                        "State {} has an epsilon transition to itself",
                        state_id
                    )
                }
            }
        }
    }

    pub fn compute_epsilon_closure<I: IntoIterator<Item = StateId>>(
        &self,
        starting_state_ids: I,
    ) -> HashSet<StateId> {
        let mut state_ids = HashSet::new();

        let mut stack: Vec<_> = starting_state_ids.into_iter().collect();

        while let Some(state_id) = stack.pop() {
            state_ids.insert(state_id);

            let state = self.states.get(&state_id).expect("state should exist");

            for (transition, next_state_id) in &state.transitions {
                if !matches!(transition, Transition::Epsilon) || state_ids.contains(next_state_id) {
                    continue;
                }

                stack.push(*next_state_id);
            }
        }

        state_ids
    }
}

impl std::fmt::Debug for NFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state_ids: Vec<_> = self.states.keys().cloned().collect();
        state_ids.sort();

        writeln!(f, "(NFA)")?;

        for state_id in state_ids {
            let state = self.states.get(&state_id).expect("state should exist");
            write!(f, "State {} {:?} {:?}: ", state_id, state.tag, state.kind)?;

            if state.transitions.is_empty() {
                writeln!(f, "∅")?;
                continue;
            }

            for (i, t) in state.transitions.iter().enumerate() {
                write!(f, "{:?}", t)?;

                if i < state.transitions.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

struct NFABuilder {
    states: HashMap<StateId, State>,
    state_id_counter: usize,
}

impl NFABuilder {
    fn new() -> Self {
        let mut builder = NFABuilder {
            state_id_counter: 0,
            states: HashMap::new(),
        };

        let start_state_id = State::new(StateKind::NotAccepting);
        builder
            .states
            .insert(builder.start_state_id(), start_state_id);

        builder
    }

    fn start_state_id(&self) -> StateId {
        0
    }

    fn end_state_id(&self) -> StateId {
        self.state_id_counter
    }

    fn create_state(&mut self, kind: StateKind) -> StateId {
        self.state_id_counter += 1;
        let id = self.state_id_counter;
        let state = State::new(kind);
        self.states.insert(id, state);
        id
    }

    fn add_transition(&mut self, from: StateId, to: StateId, transition: Transition) {
        let state = self
            .states
            .get_mut(&from)
            .expect("'from' state should exist");
        state.add_transition(transition, to);
    }

    fn append_nfa(&mut self, state_id: StateId, nfa: NFA) {
        let offset = self.end_state_id();
        let mut reindexed_nfa = nfa.reindex_states(offset);
        let nfa_start = reindexed_nfa
            .states
            .remove(&reindexed_nfa.start_state_id())
            .expect("NFA start state not found");

        self.states
            .entry(state_id)
            .and_modify(|state| state.transitions.extend(nfa_start.transitions));

        self.states.extend(reindexed_nfa.states);
        self.state_id_counter = reindexed_nfa.end_state_id;
    }

    fn build(self) -> NFA {
        let nfa = NFA {
            end_state_id: self.end_state_id(),
            states: self.states,
        };
        nfa.assert_valid();
        nfa
    }
}

fn visit_hir(hir: &Hir) -> Result<NFA, UnsupportedFeature> {
    match hir.kind() {
        HirKind::Empty => Err(UnsupportedFeature::EmptyPattern),
        HirKind::Look(_) => Err(UnsupportedFeature::LookaheadPattern),
        HirKind::Literal(literal) => Ok(visit_literal(literal)),
        HirKind::Class(class) => match class {
            Class::Unicode(class) => Ok(visit_class(class)),
            Class::Bytes(_) => Err(UnsupportedFeature::ByteCharClass),
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

    let mut parent_id = builder.start_state_id();

    for (idx, c) in literal.chars().enumerate() {
        let kind = if idx == literal.len() - 1 {
            StateKind::Accepting
        } else {
            StateKind::NotAccepting
        };

        let id = builder.create_state(kind);
        builder.add_transition(parent_id, id, Transition::Range(c.into()));
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
            builder.start_state_id(),
            state,
            Transition::Range(if start == end {
                start.into()
            } else {
                (start, end).into()
            }),
        );
    }

    builder.build()
}

fn visit_repetition(repetition: &Repetition) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

    let nfa = visit_hir(&repetition.sub)?;

    if repetition.min > 0 {
        let mut nfa_not_accepting = nfa.clone();
        nfa_not_accepting.convert_states_to_not_accepting();

        // first min-1 NFAs must not accept transformations
        for _ in 1..repetition.min {
            builder.append_nfa(builder.end_state_id(), nfa_not_accepting.clone());
        }

        // last NFA to meet the min requirement
        builder.append_nfa(builder.end_state_id(), nfa.clone());
    }

    if let Some(max) = repetition.max {
        let mut intermediary_state_ids = vec![builder.end_state_id()];

        // unwind all other repetitions till max
        for _ in repetition.min..max {
            builder.append_nfa(builder.end_state_id(), nfa.clone());
            let state_after_id = builder.create_state(StateKind::Accepting);
            builder.add_transition(state_after_id - 1, state_after_id, Transition::Epsilon);
            intermediary_state_ids.push(state_after_id);
        }

        // connect all intermediary states to the end state
        let end_state_id = builder.end_state_id();
        for intermediary_state_id in intermediary_state_ids {
            if intermediary_state_id != end_state_id {
                builder.add_transition(intermediary_state_id, end_state_id, Transition::Epsilon);
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
        builder.add_transition(
            builder.end_state_id(),
            state_before_loop,
            Transition::Epsilon,
        );

        let end_state = builder.create_state(StateKind::Inherit);
        builder.add_transition(state_before_loop, end_state, Transition::Epsilon);
    }

    if !repetition.greedy {
        // if repetition is not greedy, we can jump dirrectly to the end state
        builder.add_transition(
            builder.start_state_id(),
            builder.end_state_id(),
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
            nfa.convert_states_to_not_accepting();
        }

        builder.append_nfa(builder.end_state_id(), nfa);
    }

    Ok(builder.build())
}

fn visit_alternation(hirs: &[Hir]) -> Result<NFA, UnsupportedFeature> {
    let mut builder = NFABuilder::new();

    let end_states = hirs
        .iter()
        .map(|hir| {
            let nfa = visit_hir(hir)?;
            builder.append_nfa(builder.start_state_id(), nfa);
            Ok(builder.end_state_id())
        })
        .collect::<Result<Vec<_>, _>>()?;

    // reconnect dangling ending states of inner NFAs
    let end_state = builder.create_state(StateKind::Inherit);
    for inner_end_state in end_states {
        builder.add_transition(inner_end_state, end_state, Transition::Epsilon);
    }

    Ok(builder.build())
}

// TODO:
// #[cfg(test)]
// mod tests {
// }
