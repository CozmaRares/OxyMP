use std::collections::{HashMap, HashSet};

use crate::range::{Range, Ranges};

use super::nfa;

#[derive(Debug, Clone)]
pub struct Transition(pub Range);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
}

#[derive(Debug, Clone)]
pub enum StateKind {
    NotAccepting,
    Accepting(StateTag),
}

type StateId = usize;

#[derive(Debug, Clone)]
pub struct State {
    pub transitions: Vec<(Transition, StateId)>,
    pub kind: StateKind,
}

#[derive(Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct DFA {
    states: HashMap<StateId, State>,
}

// FIX: DFA can't compile properly after refactoring to ranges
pub fn compile(nfa: nfa::NFA) -> DFA {
    let alphabet = compute_alphabet(&nfa);
    let mut builder = DFABuilder::new(&nfa);

    let nfa_starting_state_ids = nfa.compute_epsilon_closure([nfa.start_state_id()]);
    builder.create_state(nfa_starting_state_ids);

    while !builder.unmarked_state_ids.is_empty() {
        let state_id = *builder.unmarked_state_ids.iter().next().unwrap();
        builder.unmarked_state_ids.remove(&state_id);
        let dfa_state_nfa_equivalent_states = builder
            .states
            .get(&state_id)
            .expect("DFA state should exist")
            .nfa_equivalent_state_ids
            .clone();

        for range in &alphabet {
            let nfa_next_state_ids =
                simulate_nfa_transition(&nfa, &dfa_state_nfa_equivalent_states, range);

            let nfa_next_state_ids = nfa.compute_epsilon_closure(nfa_next_state_ids);

            if nfa_next_state_ids.is_empty() {
                continue;
            }

            let next_state_id = match builder.get_equivalent_state_id(&nfa_next_state_ids) {
                None => builder.create_state(nfa_next_state_ids),
                Some(id) => id,
            };

            builder.add_transition(state_id, next_state_id, range.clone());
        }
    }

    let dfa = builder.build();
    let dfa = minimize(dfa);
    compress_char_classes(dfa)
}

// TODO:
fn minimize(dfa: DFA) -> DFA {
    dfa
}

fn compress_char_classes(mut dfa: DFA) -> DFA {
    dfa.states = dfa
        .states
        .into_iter()
        .map(|(id, mut state)| {
            // group transitions by target state
            let mut transitions: HashMap<StateId, Vec<Range>> = HashMap::new();

            for (Transition(range), target) in state.transitions {
                transitions.entry(target).or_default().push(range);
            }

            state.transitions = transitions
                .into_iter()
                .flat_map(|(target, ranges)| {
                    ranges
                        .aggregate_ranges()
                        .into_iter()
                        .map(move |range| (Transition(range), target))
                })
                .collect();

            (id, state)
        })
        .collect();

    dfa
}

fn compute_alphabet(nfa: &nfa::NFA) -> Vec<Range> {
    nfa.states()
        .values()
        .flat_map(|state| &state.transitions)
        .filter_map(|(transition, _)| match transition {
            nfa::Transition::Range(range) => Some(range.clone()),
            _ => None,
        })
        .split_ranges()
}

impl From<char> for Transition {
    fn from(c: char) -> Self {
        Transition(c.into())
    }
}

impl From<(char, char)> for Transition {
    fn from(tuple: (char, char)) -> Self {
        Transition(tuple.into())
    }
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

    fn add_transition(&mut self, transition: Transition, target_id: StateId) {
        self.transitions.push((transition, target_id));
    }
}

impl DFA {
    #[inline]
    pub fn states(&self) -> &HashMap<StateId, State> {
        &self.states
    }

    fn assert_valid(&self) {
        // State 1 is always the start state
        let start_state_id = 1;

        oxymp_assert!(
            self.states.contains_key(&start_state_id),
            "DFA start state not found"
        );

        oxymp_assert!(
            !self
                .states
                .get(&start_state_id)
                .unwrap()
                .transitions
                .is_empty(),
            "DFA start state must have at least one transition"
        );

        for (state_id, state) in &self.states {
            for (_, next_state_id) in &state.transitions {
                oxymp_assert!(
                    self.states.contains_key(next_state_id),
                    "Transition target state {} not found",
                    next_state_id
                );

                oxymp_assert!(
                    *next_state_id != start_state_id,
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
            let state = self.states.get(&state_id).expect("state should exist");
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

        Ok(())
    }
}

struct BuilderState {
    nfa_equivalent_state_ids: HashSet<StateId>,
    state: State,
}

impl BuilderState {
    fn new(nfa_equivalent_state_ids: HashSet<StateId>, state: State) -> Self {
        Self {
            nfa_equivalent_state_ids,
            state,
        }
    }

    fn add_transition(&mut self, transition: Transition, target_id: StateId) {
        self.state.add_transition(transition, target_id);
    }
}

struct DFABuilder<'a> {
    nfa: &'a nfa::NFA,
    states: HashMap<StateId, BuilderState>,
    state_id_counter: usize,
    unmarked_state_ids: HashSet<StateId>,
}

impl<'a> DFABuilder<'a> {
    fn new(nfa: &'a nfa::NFA) -> Self {
        DFABuilder {
            nfa,
            state_id_counter: 0,
            states: HashMap::new(),
            unmarked_state_ids: HashSet::new(),
        }
    }

    fn create_state(&mut self, nfa_equivalent_state_ids: HashSet<StateId>) -> StateId {
        let nfa_states: Vec<_> = self
            .nfa
            .states()
            .iter()
            .filter(|(state_id, _)| nfa_equivalent_state_ids.contains(state_id))
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
        let state = BuilderState::new(nfa_equivalent_state_ids, State::new(kind));
        self.states.insert(id, state);
        self.unmarked_state_ids.insert(id);
        id
    }

    fn add_transition(&mut self, from: StateId, to: StateId, range: Range) {
        let state = self
            .states
            .get_mut(&from)
            .expect("'from' state should exist");
        state.add_transition(Transition(range), to);
    }

    // FIX: this is not performant ~ O(n^3)
    fn get_equivalent_state_id(&self, nfa_equivalent_state_ids: &HashSet<StateId>) -> Option<StateId> {
        for (id, state) in &self.states {
            if state.nfa_equivalent_state_ids == *nfa_equivalent_state_ids {
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

fn simulate_nfa_transition<'a, I: IntoIterator<Item = &'a StateId>>(
    nfa: &nfa::NFA,
    starting_state_ids: I,
    range: &Range,
) -> HashSet<StateId> {
    let mut state_ids = HashSet::new();

    for state_id in starting_state_ids {
        let state = nfa.states().get(state_id).expect("state not found");

        for (transition, next_state_id) in &state.transitions {
            match transition {
                nfa::Transition::Range(transition_range) => {
                    if transition_range.contains_range(range) {
                        state_ids.insert(*next_state_id);
                    }
                }
                _ => {}
            }
        }
    }

    state_ids
}

// TODO:
// #[cfg(test)]
// mod tests {
// }
