use parser_combinators::{token, optional, satisfy, many1, parser, ParserExt};
use parser_combinators::primitives::{ParseResult, State, Stream, Positioner, Parser, from_iter};
use tokenizer::{self, AST};
use interpolate;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Op {
    Cmd {
        prog: String,
        args: Vec<String>,
    },
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ASTPositioner(usize);
impl Positioner for AST {
    type Position = ASTPositioner;

    fn start() -> ASTPositioner {
        ASTPositioner(0)
    }

    fn update(&self, p: &mut ASTPositioner) {
        p.0 += 1;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpPositioner(usize);
impl Positioner for Op {
    type Position = OpPositioner;

    fn start() -> OpPositioner {
        OpPositioner(0)
    }

    fn update(&self, p: &mut OpPositioner) {
        p.0 += 1;
    }
}

fn args<I>(input: State<I>) -> ParseResult<Vec<Vec<AST>>, I, AST>
where I: Stream<Item=AST> {
    many1(many1(satisfy(|t| t != AST::Whitespace))
          .skip(optional(token(AST::Whitespace)))
         )
        .parse_state(input)
}

fn command<I>(input: State<I>) -> ParseResult<Op, I, AST>
where I: Stream<Item=AST> {
    optional(token(AST::Whitespace))
        .with(parser(args::<I>))
        .skip(optional(token(AST::Whitespace)))
        .map(|args: Vec<Vec<AST>>| {
            let mut string_args: Vec<String> = args
            .iter()
            .map(interpolate::expand).collect();
            let prog = string_args.remove(0);
            Op::Cmd {
                prog: prog,
                args: string_args,
            }
        })
    .parse_state(input)
}

pub fn parse(line: &str) -> Result<Option<Op>, String> {
    let tokens: Vec<AST> = try!(tokenizer::tokenize(&line));
    println!("{:?}", tokens);
    if tokens.is_empty() {
        return Ok(None);
    }
    parser(command).parse(from_iter(tokens.iter().cloned()))
        .map(|r| Some(r.0))
        .or_else(|e| Err(format!("{:?}", e.errors)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty() {
        let cmd = parse("");
        assert_eq!(cmd, Ok(None));
    }

    #[test]
    fn parse_cmd_no_args() {
        let cmd = parse("cmd").unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: Vec::new(),
        });
    }

    #[test]
    fn parse_cmd_multiple_args() {
        let cmd = parse("cmd arg1 arg2").unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: vec!["arg1".to_string(), "arg2".to_string()],
        });
    }

    #[test]
    fn parse_cmd_with_string_arg() {
        let cmd = parse(r#"cmd "arg1 $VAR arg2""#).unwrap().unwrap();
        assert_eq!(cmd, Op::Cmd {
            prog: "cmd".to_string(),
            args: vec!["arg1  arg2".to_string()],
        });
    }
}
