use std::collections::{HashMap, HashSet};

use super::nfa;

#[derive(Debug, Clone)]
pub enum Transition {
    Char(char),
    Chars { start: char, end: char },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StateTag {
    Skip { pattern: String },
    Token { variant: String, priority: usize },
}

#[derive(Debug, Clone)]
pub enum StateKind {
    NotAccepting,
    Accepting(StateTag),
}

#[derive(Debug, Clone)]
pub struct State {
    pub transitions: Vec<(Transition, usize)>,
    pub kind: StateKind,
}

#[derive(Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct DFA {
    states: HashMap<usize, State>,
}

pub fn compile(nfa: nfa::NFA) -> DFA {
    let alphabet = compute_alphabet(&nfa);
    let mut builder = DFABuilder::new(&nfa);

    let nfa_starting_states = nfa.compute_epsilon_closure([nfa.start_state()]);
    builder.create_state(nfa_starting_states);

    while !builder.unmarked_states.is_empty() {
        let state_id = *builder.unmarked_states.iter().next().unwrap();
        builder.unmarked_states.remove(&state_id);
        let dfa_state_nfa_equivalent_states = builder
            .states
            .get(&state_id)
            .expect("DFA state should exist")
            .nfa_equivalent_states
            .clone();

        for letter in &alphabet {
            let nfa_next_states =
                nfa.simulate_transition(&dfa_state_nfa_equivalent_states, *letter);
            let nfa_next_states = nfa.compute_epsilon_closure(nfa_next_states);

            if nfa_next_states.is_empty() {
                continue;
            }

            let next_state_id = match builder.get_equivalent_state(&nfa_next_states) {
                None => builder.create_state(nfa_next_states),
                Some(id) => id,
            };

            builder.add_transition(state_id, next_state_id, Transition::Char(*letter));
        }
    }

    let dfa = builder.build();
    let dfa = minimize(dfa);
    compress_char_classes(dfa)
}

// TODO:
pub fn minimize(dfa: DFA) -> DFA {
    dfa
}

fn compress_char_classes(mut dfa: DFA) -> DFA {
    dfa.states = dfa
        .states
        .into_iter()
        .map(|(id, mut state)| {
            // group transitions by target state
            let mut transitions: HashMap<usize, Vec<char>> = HashMap::new();

            for (transition, target) in &state.transitions {
                match transition {
                    Transition::Char(c) => {
                        transitions.entry(*target).or_default().extend([*c]);
                    }
                    Transition::Chars { start, end } => {
                        for c in *start..=*end {
                            transitions.entry(*target).or_default().extend([c]);
                        }
                    }
                }
            }

            transitions.iter_mut().for_each(|(_, chars)| chars.sort());

            let mut compressed_transitions: HashMap<usize, Vec<Transition>> = HashMap::new();

            for (target, chars) in transitions {
                let mut chars = chars.iter().peekable();
                let mut compressed_transitions_vec = Vec::new();

                while let Some(current_char) = chars.next() {
                    match chars.peek() {
                        Some(next_char) => {
                            if (*current_char as u64) + 1 == **next_char as u64 {
                                let start_char = *current_char;
                                let mut end_char = *current_char;

                                while let Some(lookahead_char) = chars.peek() {
                                    if (end_char as u64) + 1 == **lookahead_char as u64 {
                                        end_char = **lookahead_char;
                                        chars.next();
                                    } else {
                                        break;
                                    }
                                }

                                compressed_transitions_vec.push(Transition::Chars {
                                    start: start_char,
                                    end: end_char,
                                });
                            } else {
                                compressed_transitions_vec.push(Transition::Char(*current_char));
                            }
                        }
                        None => {
                            compressed_transitions_vec.push(Transition::Char(*current_char));
                        }
                    }
                }

                compressed_transitions.insert(target, compressed_transitions_vec);
            }

            state.transitions = compressed_transitions
                .into_iter()
                .flat_map(|(target, transitions)| {
                    transitions
                        .into_iter()
                        .map(move |transition| (transition, target))
                })
                .collect();

            (id, state)
        })
        .collect();

    dfa
}

fn compute_alphabet(nfa: &nfa::NFA) -> HashSet<char> {
    let mut alphabet = HashSet::new();

    for state in nfa.states().values() {
        for (transition, _) in &state.transitions {
            match transition {
                nfa::Transition::Char(c) => {
                    alphabet.insert(*c);
                }
                nfa::Transition::Chars { start, end } => {
                    for c in *start..=*end {
                        alphabet.insert(c);
                    }
                }
                nfa::Transition::Epsilon => {}
            }
        }
    }

    alphabet
}

impl TryFrom<nfa::StateTag> for StateTag {
    type Error = ();

    fn try_from(value: nfa::StateTag) -> Result<Self, Self::Error> {
        match value {
            nfa::StateTag::None => Err(()),
            nfa::StateTag::Token { variant, priority } => Ok(StateTag::Token { variant, priority }),
            nfa::StateTag::Skip { pattern } => Ok(StateTag::Skip { pattern }),
        }
    }
}

impl StateKind {
    fn accept_if_higher_priority(self, new_tag: StateTag) -> Self {
        let tag = match self {
            StateKind::NotAccepting => new_tag,
            StateKind::Accepting(current_tag) => match (&current_tag, &new_tag) {
                (StateTag::Skip { .. }, StateTag::Token { .. }) => new_tag,
                (
                    StateTag::Token {
                        priority: current_priority,
                        ..
                    },
                    StateTag::Token {
                        priority: new_priority,
                        ..
                    },
                ) if *current_priority > *new_priority => new_tag,
                _ => current_tag,
            },
        };

        StateKind::Accepting(tag)
    }
}

impl State {
    fn new(kind: StateKind) -> Self {
        Self {
            transitions: Vec::new(),
            kind,
        }
    }

    fn add_transition(&mut self, transition: Transition, target: usize) {
        self.transitions.push((transition, target));
    }
}

impl DFA {
    #[inline]
    pub fn states(&self) -> &HashMap<usize, State> {
        &self.states
    }

    fn assert_valid(&self) {
        let start_state = 1;

        assert!(
            self.states.contains_key(&start_state),
            "DFA start state not found"
        );

        assert!(
            !self
                .states
                .get(&start_state)
                .unwrap()
                .transitions
                .is_empty(),
            "DFA start state must have at least one transition"
        );

        for (state_id, state) in &self.states {
            assert!(
                state_id >= &start_state,
                "State {} must have a higher id than the start state",
                state_id
            );

            for (_, next_state) in &state.transitions {
                assert!(
                    self.states.contains_key(next_state),
                    "Transition target state {} not found",
                    next_state
                );

                assert!(
                    *next_state != start_state,
                    "State {} can't have a transition to the start state",
                    state_id
                );
            }
        }
    }
}

impl std::fmt::Debug for DFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state_ids: Vec<_> = self.states.keys().cloned().collect();
        state_ids.sort();

        writeln!(f, "(DFA)")?;

        for state_id in state_ids {
            if let Some(state) = self.states.get(&state_id) {
                write!(f, "State {} {:?}: ", state_id, state.kind)?;

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
        }

        Ok(())
    }
}

struct BuilderState {
    nfa_equivalent_states: HashSet<usize>,
    state: State,
}

impl BuilderState {
    fn new(nfa_equivalent_states: HashSet<usize>, state: State) -> Self {
        Self {
            nfa_equivalent_states,
            state,
        }
    }

    fn add_transition(&mut self, transition: Transition, target: usize) {
        self.state.add_transition(transition, target);
    }
}

struct DFABuilder<'a> {
    nfa: &'a nfa::NFA,
    states: HashMap<usize, BuilderState>,
    state_id_counter: usize,
    unmarked_states: HashSet<usize>,
}

impl<'a> DFABuilder<'a> {
    fn new(nfa: &'a nfa::NFA) -> Self {
        DFABuilder {
            nfa,
            state_id_counter: 0,
            states: HashMap::new(),
            unmarked_states: HashSet::new(),
        }
    }

    fn create_state(&mut self, nfa_equivalent_states: HashSet<usize>) -> usize {
        let nfa_states: Vec<_> = self
            .nfa
            .states()
            .iter()
            .filter(|(state_id, _)| nfa_equivalent_states.contains(state_id))
            .collect();

        let mut kind = StateKind::NotAccepting;

        for (_, state) in &nfa_states {
            if matches!(state.kind, nfa::StateKind::Accepting) {
                let tag = state
                    .tag
                    .clone()
                    .try_into()
                    .expect("NFA accepting state doesn't have a tag");

                kind = kind.accept_if_higher_priority(tag);
            }
        }

        self.state_id_counter += 1;
        let id = self.state_id_counter;
        let state = BuilderState::new(nfa_equivalent_states, State::new(kind));
        self.states.insert(id, state);
        self.unmarked_states.insert(id);
        id
    }

    fn add_transition(&mut self, from: usize, to: usize, transition: Transition) {
        if let Some(state) = self.states.get_mut(&from) {
            state.add_transition(transition, to);
        }
    }

    // FIX: this is not performant ~ O(n^3)
    fn get_equivalent_state(&self, nfa_equivalent_states: &HashSet<usize>) -> Option<usize> {
        for (id, state) in &self.states {
            if state.nfa_equivalent_states == *nfa_equivalent_states {
                return Some(*id);
            }
        }

        None
    }

    fn build(self) -> DFA {
        let dfa = DFA {
            states: self
                .states
                .into_iter()
                .map(|(id, state)| (id, state.state))
                .collect(),
        };
        dfa.assert_valid();
        dfa
    }
}
