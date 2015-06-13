use parser_combinators::{spaces, many1, satisfy, parser, between, optional,
    Parser, ParseResult, ParseError, ParserExt};
use parser_combinators::primitives::{State, Stream};

#[derive(Debug, PartialEq)]
pub enum AST {
    Word(String)
}

fn word<I>(input: State<I>) -> ParseResult<AST, I> where I: Stream<Item=char> {
    many1(satisfy(|c: char| !c.is_whitespace()))
        .map(AST::Word)
        .parse_state(input)
}

fn command<I>(input: State<I>) -> ParseResult<Vec<AST>, I> where I: Stream<Item=char> {
    many1(parser(word::<I>).skip(spaces()))
        .parse_state(input)
}

pub fn parse_command(input: &str) -> Result<Option<Vec<AST>>, ParseError<char>> {
    optional(between(spaces(), spaces(), parser(command)))
        .parse(input)
        .map(|x| x.0)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use super::*;

    #[test]
    fn parse_single_word() {
        let word = parse_command("cmd").unwrap().unwrap();
        assert_vec_eq(word, vec![to_word("cmd")]);
    }

    #[test]
    fn parse_multiple_words() {
        let word = parse_command("hello world").unwrap().unwrap();
        assert_vec_eq(word, vec![to_word("hello"), to_word("world")]);
    }

    #[test]
    fn parse_leading_ws() {
        let word = parse_command("    cmd").unwrap().unwrap();
        assert_vec_eq(word, vec![to_word("cmd")]);
    }

    #[test]
    fn parse_trailing_ws() {
        let word = parse_command("cmd    ").unwrap().unwrap();
        assert_vec_eq(word, vec![to_word("cmd")]);
    }

    #[test]
    fn parse_empty_cmd() {
        let word = parse_command("").unwrap();
        assert_eq!(word, None);
    }

    fn to_word(s: &str) -> AST {
        AST::Word(s.to_string())
    }

    fn assert_vec_eq<T: PartialEq + Debug>(v1: Vec<T>, v2: Vec<T>) {
        assert_eq!(v1.len(), v2.len());
        for i in (0..v1.len()) {
            assert_eq!(v1[i], v2[i]);
        }
    }
}
