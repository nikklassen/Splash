use combine::parser::char::{char, string};
use combine::*;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug)]
struct MatchState<'a> {
    pub s: &'a [char],
    pub s_pos: usize,
    pub pattern: &'a [char],
    pub pattern_pos: usize,
}

#[derive(Clone, Debug)]
enum BracketGroup {
    CharRange(char, char),
    CharSet(HashSet<char>),
    Inverted(Box<BracketGroup>),
    Multi(Vec<BracketGroup>),
}

macro_rules! hash_set {
    ($($item: expr),*) => {{
        let mut set = HashSet::new();
        $(set.insert($item);)*
        set
    }}
}

parser! {
    fn group_char[I]()(I) -> char
    where [I: Stream<Item = char>] {
        satisfy(|c| c != ']' && c != '-')
    }
}

parser! {
    fn char_range[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        try(group_char().skip(char('-')))
            .and(group_char().or(look_ahead(char(']')).with(value(']'))))
            .map(|(c1, c2)| {
                if c2 == ']' {
                    BracketGroup::CharSet(hash_set!(c1, '-'))
                } else {
                    BracketGroup::CharRange(c1, c2)
                }
            })
    }
}

parser! {
    fn char_class[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        let digits = BracketGroup::CharRange('0', '9');
        let az = BracketGroup::CharRange('a', 'z');
        #[allow(non_snake_case)]
        let AZ = BracketGroup::CharRange('a', 'z');
        let control = BracketGroup::CharRange(1 as char, 31 as char);
        let graphic = BracketGroup::Inverted(Box::new(control.clone()));
        let space = BracketGroup::CharSet(hash_set!('\r', '\n', ' ', '\t', 11 as char, 12 as char));
        char('[').with(
            choice![
                string(":digit:]").with(value(digits.clone())),
                string(":alnum:]").with(value(
                    BracketGroup::Multi(vec![
                        digits.clone(),
                        az.clone(),
                        AZ.clone(),
                    ]))),
                string(":upper:]").with(value(AZ.clone())),
                string(":lower:]").with(value(az.clone())),
                string(":alpha:]").with(value(
                    BracketGroup::Multi(vec![
                        az.clone(),
                        AZ.clone(),
                    ]))),
                string(":lower:]").with(value(space.clone())),
                string(":cntrl:]").with(value(control.clone())),
                string(":print:]").with(value(BracketGroup::Multi(vec![graphic.clone(), space]))),
                string(":blank:]").with(value(BracketGroup::CharSet(hash_set!(' ', '\t')))),
                string(":graph:]").with(value(graphic)),
                string(":punct:]").with(value(BracketGroup::Multi(vec![
                    BracketGroup::CharRange('!', '/'),
                    BracketGroup::CharRange(':', '@'),
                    BracketGroup::CharRange('[', '`'),
                    BracketGroup::CharRange('{', 127 as char),
                ]))),
                string(":xdigit:]").with(value(
                    BracketGroup::Multi(vec![
                        digits.clone(),
                        BracketGroup::CharRange('a', 'f'),
                        BracketGroup::CharRange('A', 'F'),
                    ]))),
                unexpected("invalid char class").map(|_| unreachable!())
            ])
    }
}

parser! {
    fn char_set[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        many1(try(group_char().skip(not_followed_by(char('-')))))
            .map(|s: HashSet<char>| BracketGroup::CharSet(s))
    }
}

parser! {
    fn multi_group[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        optional(char('-'))
            .and(many(char_class().or(char_set()).or(char_range())))
            .map(|(dash_opt, mut gs): (Option<char>, Vec<BracketGroup>)| {
                if dash_opt.is_some() {
                    gs.insert(0, BracketGroup::CharSet(hash_set!('-')));
                }
                BracketGroup::Multi(gs)
            })
    }
}

parser! {
    fn inverted_group[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        char('!').with(multi_group()).map(|g| BracketGroup::Inverted(Box::new(g)))
    }
}

parser! {
    fn bracket_group[I]()(I) -> BracketGroup
    where [I: Stream<Item = char>] {
        between(
            char('['),
            char(']'),
            choice![
                inverted_group(),
                multi_group()
            ])
    }
}

fn get_bracket_group(s: &[char]) -> Result<(BracketGroup, usize), String> {
    if s.iter().find(|c| **c == ']').is_none() {
        return Ok((BracketGroup::CharSet(hash_set!('[')), 1));
    }
    bracket_group()
        .easy_parse(s)
        .map(|(bg, remaining)| (bg, s.len() - remaining.len()))
        .map_err(|err| {
            let err = err
                .map_range(|r| format!("{:?}", r))
                .map_position(|p| p.translate_position(s));
            format!("{}\nIn input: `{:?}`", err, s)
        })
}

fn matches_bracket_group(c: char, bg: &BracketGroup) -> bool {
    match bg {
        BracketGroup::Multi(bgs) => bgs.iter().any(|bg| matches_bracket_group(c, bg)),
        BracketGroup::CharRange(c1, c2) => *c1 <= c && c <= *c2,
        BracketGroup::CharSet(s) => s.contains(&c),
        BracketGroup::Inverted(bg) => !matches_bracket_group(c, bg),
    }
}

fn match_char(mut state: MatchState) -> Result<bool, String> {
    let end_of_pattern = state.pattern_pos >= state.pattern.len();
    let end_of_string = state.s_pos >= state.s.len();
    if end_of_string || end_of_pattern {
        return Ok(end_of_pattern);
    }

    let original_state = state;
    let r = match state.pattern[state.pattern_pos] {
        '*' => {
            state.pattern_pos += 1;
            if !match_char(state)? {
                state = original_state;
                state.s_pos += 1;
                match_char(state)?
            } else {
                true
            }
        }
        '?' => {
            state.s_pos += 1;
            state.pattern_pos += 1;
            match_char(state)?
        }
        '[' => {
            let (bg, end) = get_bracket_group(&state.pattern[state.pattern_pos..])?;
            if matches_bracket_group(state.s[state.s_pos], &bg) {
                state.s_pos += 1;
                state.pattern_pos += end;
                match_char(state)?
            } else {
                false
            }
        }
        c => {
            if state.s[state.s_pos] == c {
                state.pattern_pos += 1;
                state.s_pos += 1;
                match_char(state)?
            } else {
                false
            }
        }
    };
    if !r && original_state.pattern_pos == 0 {
        state = original_state;
        state.s_pos += 1;
        match_char(state)
    } else {
        Ok(r)
    }
}

pub fn is_match(s: &str, pattern: &str) -> Result<bool, String> {
    let s_chars = s.chars().collect::<Vec<char>>();
    let pattern_chars = pattern.chars().collect::<Vec<char>>();
    match_char(MatchState {
        s: &s_chars[..],
        s_pos: 0,
        pattern: &pattern_chars[..],
        pattern_pos: 0,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn star() {
        assert!(is_match("abbc", "a*c").unwrap());
    }

    #[test]
    fn star_no_match() {
        assert!(!is_match("abbc", "a*d").unwrap());
    }

    #[test]
    fn star_greedy() {
        assert!(is_match("abbbc", "a*bc").unwrap());
    }

    #[test]
    fn star_zero_length() {
        assert!(is_match("ac", "a*c").unwrap());
    }

    #[test]
    fn question_mark() {
        assert!(is_match("abc", "a?c").unwrap());
    }

    #[test]
    fn literal() {
        assert!(is_match("abc", "abc").unwrap());
    }

    #[test]
    fn pattern_contained() {
        assert!(is_match("abcd", "bc").unwrap());
    }

    #[test]
    fn pattern_too_long() {
        assert!(!is_match("ab", "abc").unwrap());
    }

    #[test]
    fn pattern_partial_no_match() {
        assert!(!is_match("ad", "ac").unwrap());
    }

    #[test]
    fn pattern_partial_match() {
        assert!(is_match("aac", "ac").unwrap());
    }

    #[test]
    fn bracket_char_class() {
        assert!(is_match("1", "[[:digit:]]").unwrap());
    }

    #[test]
    fn bracket_char_set() {
        assert!(is_match("b", "[abc]").unwrap());
    }

    #[test]
    fn bracket_char_range() {
        assert!(is_match("b", "[a-c]").unwrap());
    }

    #[test]
    fn bracket_inverted() {
        assert!(is_match("d", "[!abc]").unwrap());
    }

    #[test]
    fn bracket_invalid_char_class() {
        assert!(is_match("d", "[[:foo:]]").is_err());
    }

    #[test]
    fn bracket_unterminated() {
        assert!(is_match("[a", "[a").unwrap());
    }

    #[test]
    fn bracket_leading_dash() {
        assert!(is_match("-", "[-a]").unwrap());
    }

    #[test]
    fn bracket_trailing_dash() {
        assert!(is_match("-", "[a-]").unwrap());
    }
}
