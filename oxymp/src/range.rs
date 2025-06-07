use std::collections::HashMap;

use crate::utils::CharHelper;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Range {
    One(char),
    Multi { start: char, end: char },
}

impl From<char> for Range {
    fn from(c: char) -> Self {
        Range::One(c)
    }
}

impl From<(char, char)> for Range {
    fn from((start, end): (char, char)) -> Self {
        oxymp_assert!(
            start <= end,
            "start char should be less than or equal to end char"
        );

        if start == end {
            return start.into();
        }

        Range::Multi { start, end }
    }
}

impl Range {
    pub fn contains_char(&self, c: &char) -> bool {
        match self {
            Range::One(this_c) => this_c == c,
            Range::Multi { start, end } => start <= c && c <= end,
        }
    }

    pub fn contains_range(&self, other: &Range) -> bool {
        match (self, other) {
            (Range::One(_), Range::Multi { .. }) => false,
            (_, Range::One(other_char)) => self.contains_char(other_char),
            (
                Range::Multi { start, end },
                Range::Multi {
                    start: other_start,
                    end: other_end,
                },
            ) => start <= other_start && other_end <= end,
        }
    }

    fn start(&self) -> char {
        match self {
            Range::One(c) => *c,
            Range::Multi { start, .. } => *start,
        }
    }

    fn end(&self) -> char {
        match self {
            Range::One(c) => *c,
            Range::Multi { end, .. } => *end,
        }
    }
}

pub trait Ranges {
    fn split_ranges(self) -> Vec<Range>;
    fn aggregate_ranges(self) -> Vec<Range>;
}

impl<Iter: IntoIterator<Item = Range>> Ranges for Iter {
    fn split_ranges(self) -> Vec<Range> {
        let mut events = compute_events(self);
        events = split_overlapping_events(events);
        make_ranges(events)
    }

    fn aggregate_ranges(self) -> Vec<Range> {
        let mut events = compute_events(self);
        events = remove_nested_events(events);
        let ranges = make_ranges(events);
        combine_adjacent_ranges(ranges)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
enum EventType {
    Start,
    End,
}

struct Event {
    event_type: EventType,
    occurrences: usize,
    value: char,
}

impl Event {
    fn new(event_type: EventType, value: char, occurrences: usize) -> Self {
        Self {
            event_type,
            occurrences,
            value,
        }
    }

    fn start(value: char) -> Self {
        Self {
            event_type: EventType::Start,
            occurrences: 1,
            value,
        }
    }

    fn end(value: char) -> Self {
        Self {
            event_type: EventType::End,
            occurrences: 1,
            value,
        }
    }

    fn get_tuple(&self) -> (char, EventType) {
        (self.value, self.event_type.clone())
    }
}

impl std::fmt::Debug for Event {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({:?}, {:?}, {:?})",
            self.value, self.event_type, self.occurrences
        )
    }
}

impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.get_tuple() == other.get_tuple()
    }
}
impl Eq for Event {}

impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Event {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_tuple().cmp(&other.get_tuple())
    }
}

fn assert_non_duplicate_events(events: &[Event]) {
    if events.is_empty() {
        return;
    }

    let mut iter = events.iter();
    let mut prev_event = iter.next().unwrap();

    oxymp_assert!(
        prev_event.event_type == EventType::Start,
        "first event must be a start event"
    );

    for event in iter {
        oxymp_assert!(
            event != prev_event,
            "event {}({}) is equal to the previous event {}({})",
            event.value,
            event.value as u32,
            prev_event.value,
            prev_event.value as u32,
        );
        prev_event = event;
    }
}

// events should come paris, in sorted order
fn assert_correct_events(events: &[Event]) {
    if events.is_empty() {
        return;
    }

    let mut iter = events.iter().enumerate();
    let mut prev_event = iter.next().unwrap().1;

    oxymp_assert!(
        prev_event.event_type == EventType::Start,
        "first event must be a start event"
    );

    for (idx, event) in iter {
        match (idx % 2 == 0, &event.event_type) {
            (true, EventType::Start) => {}
            (false, EventType::End) => {}
            (true, EventType::End) => {
                oxymp_assert!(
                    false,
                    "end event {}({}) on even index {}",
                    event.value,
                    event.value as u32,
                    idx
                )
            }
            (false, EventType::Start) => {
                oxymp_assert!(
                    false,
                    "start event {}({}) on odd index {}",
                    event.value,
                    event.value as u32,
                    idx
                )
            }
        }

        if prev_event.event_type == EventType::Start && event.event_type == EventType::End {
            // only Start-End pairs are allowed to have the same character
            oxymp_assert!(
                event.value >= prev_event.value,
                "event {}({}) at idx {} is not greater than or equal to previous event {}({})",
                event.value,
                event.value as u32,
                idx,
                prev_event.value,
                prev_event.value as u32,
            );
        } else {
            oxymp_assert!(
                event.value > prev_event.value,
                "event {}({}) at idx {} is not greater than previous event {}({})",
                event.value,
                event.value as u32,
                idx,
                prev_event.value,
                prev_event.value as u32,
            );
        }

        prev_event = event;
    }
}

fn compute_events<Iter: IntoIterator<Item = Range>>(ranges: Iter) -> Vec<Event> {
    let mut events: Vec<_> = ranges
        .into_iter()
        .flat_map(|range| [Event::start(range.start()), Event::end(range.end())])
        .fold(
            HashMap::<(char, EventType), usize>::new(),
            |mut acc, event| {
                let tuple = event.get_tuple();
                acc.entry(tuple)
                    .and_modify(|occurrences| *occurrences += event.occurrences)
                    .or_insert(event.occurrences);
                acc
            },
        )
        .into_iter()
        .map(|((value, event_type), occurrences)| Event::new(event_type, value, occurrences))
        .collect();
    events.sort_unstable();
    assert_non_duplicate_events(&events);
    events
}

fn split_overlapping_events(events: Vec<Event>) -> Vec<Event> {
    if events.is_empty() {
        return events;
    }

    let mut iter = events.into_iter();
    let first_event = iter.next().unwrap();

    oxymp_assert!(
        first_event.event_type == EventType::Start,
        "first event must be a start event"
    );

    let mut prev_endpoint_value = first_event.value;
    let mut active_ranges = first_event.occurrences;
    let mut output = vec![first_event];

    for event in iter {
        oxymp_assert!(
            prev_endpoint_value <= event.value,
            "invalid event {}({}), previous event's value was {}({})",
            event.value,
            event.value as u32,
            prev_endpoint_value,
            prev_endpoint_value as u32
        );

        match (&active_ranges, &event.event_type) {
            (0, EventType::End) => {
                oxymp_assert!(
                    false,
                    "end event without start event {}({})",
                    event.value,
                    event.value as u32
                )
            }

            (0, EventType::Start) => {
                prev_endpoint_value = event.value;
                active_ranges += event.occurrences;
                output.push(event);
            }

            (_, EventType::Start) => {
                active_ranges += event.occurrences;

                // ignore if eq
                if prev_endpoint_value == event.value {
                    continue;
                }

                prev_endpoint_value = event.value;
                let prev_char = event
                    .value
                    .prev_char()
                    .expect("previous endpoint is smaller than current");
                output.push(Event::end(prev_char));
                output.push(event);
            }

            (_, EventType::End) => {
                active_ranges -= event.occurrences;

                output.push(Event::end(event.value));

                if active_ranges != 0 {
                    let next_char = event
                        .value
                        .next_char()
                        .expect("next endpoint is larger than current");
                    output.push(Event::start(next_char));
                    prev_endpoint_value = next_char;
                }
            }
        }
    }

    assert_correct_events(&output);
    output
}

fn remove_nested_events(events: Vec<Event>) -> Vec<Event> {
    if events.is_empty() {
        return events;
    }

    let mut iter = events.into_iter();
    let first_event = iter.next().unwrap();

    oxymp_assert!(
        first_event.event_type == EventType::Start,
        "first event must be a start event"
    );

    let mut prev_endpoint_value = first_event.value;
    let mut active_ranges = first_event.occurrences;
    let mut output = vec![first_event];

    for event in iter {
        oxymp_assert!(
            prev_endpoint_value <= event.value,
            "invalid event {}({}), previous event's value was {}({})",
            event.value,
            event.value as u32,
            prev_endpoint_value,
            prev_endpoint_value as u32
        );

        let temp_value = event.value;

        match (&active_ranges, &event.event_type) {
            (0, EventType::End) => {
                oxymp_assert!(
                    false,
                    "end event without start event {}({})",
                    event.value,
                    event.value as u32
                )
            }

            (0, EventType::Start) => {
                active_ranges += event.occurrences;
                output.push(event);
            }

            (_, EventType::Start) => {
                active_ranges += event.occurrences;
            }

            (_, EventType::End) => {
                active_ranges -= event.occurrences;

                if active_ranges == 0 {
                    output.push(event);
                }
            }
        }

        prev_endpoint_value = temp_value;
    }

    assert_correct_events(&output);
    output
}

fn make_ranges(events: Vec<Event>) -> Vec<Range> {
    let mut ranges = Vec::new();
    let mut iter = events.into_iter().peekable();

    while iter.peek().is_some() {
        let start_char = iter.next().unwrap().value;
        let end_char = iter
            .next()
            .expect("ranges are asserted to have an even number of events")
            .value;

        ranges.push((start_char, end_char).into());
    }

    ranges
}

fn overlap(range1: &Range, range2: &Range) -> bool {
    range1.start() <= range2.end() && range2.start() <= range1.end()
}

fn are_adjacent(range1: &Range, range2: &Range) -> bool {
    let start1 = range1.start();
    let end1 = range1.end();
    let start2 = range2.start();
    let end2 = range2.end();

    start1.is_preceded_by(&end2) || end1.is_followed_by(&start2)
}

fn combine_adjacent_ranges(ranges: Vec<Range>) -> Vec<Range> {
    if ranges.is_empty() {
        return ranges;
    }

    let mut output = Vec::new();
    let mut iter = ranges.into_iter();
    let mut current_range = iter.next().unwrap();

    for next_range in iter {
        if overlap(&current_range, &next_range) || are_adjacent(&current_range, &next_range) {
            let start = std::cmp::min(current_range.start(), next_range.start());
            let end = std::cmp::max(current_range.end(), next_range.end());
            current_range = (start, end).into();
        } else {
            output.push(current_range);
            current_range = next_range;
        }
    }

    output.push(current_range);
    output
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    fn ranges_to_char_set(ranges: &[Range]) -> HashSet<char> {
        ranges
            .iter()
            .flat_map(|range| match range {
                Range::One(c) => *c..=*c,
                Range::Multi { start, end } => *start..=*end,
            })
            .collect()
    }

    fn assert_ranges_equivalent(a: &[Range], b: &[Range]) {
        let hash_a = ranges_to_char_set(a);
        let hash_b = ranges_to_char_set(b);
        assert_eq!(hash_a, hash_b);
    }

    #[test]
    fn test_split_ranges_empty() {
        let input: Vec<Range> = vec![];
        let result = input.split_ranges();
        assert_eq!(result, vec![]);
    }

    #[test]
    fn test_split_ranges_one_char() {
        let input = vec![Range::One('a')];
        let expected = vec![Range::One('a')];
        let result = input.split_ranges();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_split_ranges_multi_char() {
        let input = vec![
            Range::Multi {
                start: 'a',
                end: 'c',
            },
            Range::Multi {
                start: 'b',
                end: 'd',
            },
        ];
        let expected = vec![
            Range::One('a'),
            Range::Multi {
                start: 'b',
                end: 'c',
            },
            Range::One('d'),
        ];
        let result = input.split_ranges();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_split_ranges_mixed() {
        let input = vec![
            Range::One('a'),
            Range::Multi {
                start: 'a',
                end: 'g',
            },
            Range::One('e'),
        ];
        let expected = vec![
            Range::One('a'),
            Range::Multi {
                start: 'b',
                end: 'd',
            },
            Range::One('e'),
            Range::Multi {
                start: 'f',
                end: 'g',
            },
        ];
        let result = input.split_ranges();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_aggregate_ranges_empty() {
        let input: Vec<Range> = vec![];
        let result = input.aggregate_ranges();
        assert_eq!(result, vec![]);
    }

    #[test]
    fn test_aggregate_ranges_one_char() {
        let input = vec![Range::One('a')];
        let result = input.aggregate_ranges();
        assert_eq!(result, vec![Range::One('a')]);
    }

    #[test]
    fn test_aggregate_ranges_consecutive() {
        let input = vec![Range::One('a'), Range::One('b'), Range::One('c')];
        let result = input.aggregate_ranges();
        assert_eq!(
            result,
            vec![Range::Multi {
                start: 'a',
                end: 'c'
            }]
        );
    }

    #[test]
    fn test_aggregate_ranges_non_consecutive() {
        let input = vec![Range::One('a'), Range::One('c'), Range::One('e')];
        let result = input.aggregate_ranges();
        assert_eq!(
            result,
            vec![Range::One('a'), Range::One('c'), Range::One('e')]
        );
    }

    #[test]
    fn test_aggregate_ranges_mixed() {
        let input = vec![
            Range::One('a'),
            Range::One('b'),
            Range::One('d'),
            Range::One('e'),
            Range::One('f'),
        ];
        let result = input.aggregate_ranges();
        assert_eq!(
            result,
            vec![
                Range::Multi {
                    start: 'a',
                    end: 'b'
                },
                Range::Multi {
                    start: 'd',
                    end: 'f'
                }
            ]
        );
    }

    #[test]
    fn test_aggregate_overlapping_ranges() {
        let input = vec![
            Range::Multi {
                start: 'a',
                end: 'd',
            },
            Range::Multi {
                start: 'b',
                end: 'e',
            },
        ];
        let expected = vec![Range::Multi {
            start: 'a',
            end: 'e',
        }];
        let result = input.aggregate_ranges();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_split_equivalence() {
        let input = vec![
            Range::One('a'),
            Range::Multi {
                start: 'a',
                end: 'd',
            },
            Range::One('e'),
            Range::Multi {
                start: 'g',
                end: 'i',
            },
            Range::Multi {
                start: 'h',
                end: 'l',
            },
        ];
        let split = input.clone().split_ranges();
        assert_ranges_equivalent(&split, &input);
    }

    #[test]
    fn test_aggregate_equivalence() {
        let input = vec![
            Range::One('a'),
            Range::Multi {
                start: 'a',
                end: 'd',
            },
            Range::One('e'),
            Range::Multi {
                start: 'g',
                end: 'i',
            },
            Range::Multi {
                start: 'h',
                end: 'l',
            },
        ];
        let aggregate = input.clone().aggregate_ranges();
        assert_ranges_equivalent(&aggregate, &input);
    }

    #[test]
    fn test_split_aggregate_equivalence() {
        let input = vec![
            Range::One('a'),
            Range::Multi {
                start: 'a',
                end: 'd',
            },
            Range::One('e'),
            Range::Multi {
                start: 'g',
                end: 'i',
            },
            Range::Multi {
                start: 'h',
                end: 'l',
            },
        ];
        let split = input.clone().aggregate_ranges();
        let aggregate = split.clone().split_ranges();

        assert_ranges_equivalent(&split, &input);
        assert_ranges_equivalent(&aggregate, &input);
        assert_ranges_equivalent(&aggregate, &split);
    }
}
