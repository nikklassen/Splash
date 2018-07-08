use super::ast::*;
use combine::parser::char::{alpha_num, char, digit, letter, spaces, string};
use combine::stream::*;
use combine::*;

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

parser! {
    fn term[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        between(
            lex(char('(')),
            lex(char(')')),
            lex(expression()))
            .or(
                lex(many1(digit()))
                    .map(|s: String| Expression::Num(s.parse::<i64>().unwrap())))
    }
}

fn binary(op_str: &str) -> impl FnOnce(Expression, Expression) -> Expression {
    let op = Op::parse(op_str).unwrap();
    move |lhs, rhs| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs))
}

parser! {
    fn prefix[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            string("!").with(value(Op::LogicalNot)),
            string("~").with(value(Op::BinNot)),
            string("+").with(value(Op::Plus)),
            string("-").with(value(Op::Minus))
        ];
        op.and(prefix()).map(|(op, e)| {
            Expression::UnaryExpression(op, Box::new(e))
        }).or(term())
    }
}

parser! {
    fn mul_div[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            string("*"),
            string("/"),
            string("%")
        ];
        chainl1(lex(prefix()), lex(op).map(|op| binary(op)))
    }
}

parser! {
    fn add_sub[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            string("+"),
            string("-")
        ];
        chainl1(lex(mul_div()), lex(op).map(|op| binary(op)))
    }
}

parser! {
    fn bit_shift[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            try(string("<<")),
            try(string(">>"))
        ];
        chainl1(lex(add_sub()), lex(op).map(|op| binary(op)))
    }
}

parser! {
    fn rel[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            string("<"),
            string("<="),
            string(">"),
            string(">=")
        ];
        chainl1(lex(bit_shift()), lex(op).map(|op| binary(op)))
    }
}

parser! {
    fn eq[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let op = choice![
            string("=="),
            string("!=")
        ];
        chainl1(lex(rel()), lex(op).map(|op| binary(op)))
    }
}

parser! {
    fn bit_and[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        chainl1(lex(eq()), lex(string("&")).map(|op| binary(op)))
    }
}

parser! {
    fn bit_xor[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        chainl1(lex(bit_and()), lex(string("^")).map(|op| binary(op)))
    }
}

parser! {
    fn bit_or[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        chainl1(lex(bit_xor()), lex(string("|")).map(|op| binary(op)))
    }
}

parser! {
    fn logical_and[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        chainl1(lex(bit_or()), lex(string("&&")).map(|op| binary(op)))
    }
}

parser! {
    fn logical_or[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        chainl1(lex(logical_and()), lex(string("||")).map(|op| binary(op)))
    }
}

parser! {
    fn ternary[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        logical_or()
            .and(optional(
                lex(string("?"))
                    .with(ternary())
                    .skip(lex(string(":")))
                    .and(ternary())))
            .map(|(e1, e2_opt)| {
                if let Some((e2, e3)) = e2_opt {
                    Expression::Conditional(Box::new(e1), Box::new(e2), Box::new(e3))
                } else {
                    e1
                }
            })
    }
}

parser! {
    fn assignment[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        let var = lex(letter().and(many(alpha_num())))
            .map(|(c, cs): (char, String)| {
                let mut s = String::new();
                s.push(c);
                s.push_str(&cs);
                s
            });
        let op = choice![
            string("=").with(value(Op::Equal)),
            string("+=").with(value(Op::Add)),
            string("-=").with(value(Op::Minus)),
            string("*=").with(value(Op::Mul)),
            string("/=").with(value(Op::Div)),
            string("%=").with(value(Op::Mod)),
            string("&=").with(value(Op::BinAnd)),
            string("|=").with(value(Op::BinOr)),
            string("^=").with(value(Op::BinXor)),
            string("<<=").with(value(Op::LShift)),
            string(">>=").with(value(Op::RShift))
        ];

        var.and(lex(op)).and(expression())
            .map(|((var, op), e)| Expression::Assignment(op, var, Box::new(e)))
    }

}

parser! {
    fn expression[I]()(I) -> Expression
    where [I: Stream<Item = char>] {
        between(
            spaces(),
            spaces(),
            assignment().or(ternary()))
    }
}

pub fn parse(e: &str) -> Result<Expression, String> {
    expression().easy_parse(e).map(|r| r.0).map_err(|err| {
        let err = err
            .map_range(|r| format!("{:?}", r))
            .map_position(|p| p.translate_position(e));
        format!("{}\nIn input: `{:?}`", err, e)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add() {
        let r = parse("1 + 1").unwrap();
        assert_eq!(
            r,
            Expression::BinaryExpression(
                Op::Add,
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(1))
            )
        );
    }

    #[test]
    fn unary_minus() {
        let r = parse("-1").unwrap();
        assert_eq!(
            r,
            Expression::UnaryExpression(Op::Minus, Box::new(Expression::Num(1)))
        );
    }

    #[test]
    fn chained_assignment() {
        let r = parse("a=b*=1").unwrap();
        assert_eq!(
            r,
            Expression::Assignment(
                Op::Equal,
                String::from("a"),
                Box::new(Expression::Assignment(
                    Op::Mul,
                    String::from("b"),
                    Box::new(Expression::Num(1))
                ))
            )
        );
    }

    #[test]
    fn nested_ternary() {
        let r = parse("1 ? 2 : 3 ? 4 : 5").unwrap();
        assert_eq!(
            r,
            Expression::Conditional(
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2)),
                Box::new(Expression::Conditional(
                    Box::new(Expression::Num(3)),
                    Box::new(Expression::Num(4)),
                    Box::new(Expression::Num(5))
                ))
            )
        );
    }

    #[test]
    fn parens() {
        let r = parse("1 * (2 + 3)").unwrap();
        assert_eq!(
            r,
            Expression::BinaryExpression(
                Op::Mul,
                Box::new(Expression::Num(1)),
                Box::new(Expression::BinaryExpression(
                    Op::Add,
                    Box::new(Expression::Num(2)),
                    Box::new(Expression::Num(3))
                ))
            )
        );
    }

    #[test]
    fn chained_unary() {
        let r = parse("-+1").unwrap();
        assert_eq!(
            r,
            Expression::UnaryExpression(
                Op::Minus,
                Box::new(Expression::UnaryExpression(
                    Op::Plus,
                    Box::new(Expression::Num(1))
                ))
            )
        );
    }

    #[test]
    fn less() {
        let r = parse("1 < 2").unwrap();
        assert_eq!(
            r,
            Expression::BinaryExpression(
                Op::Less,
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2))
            )
        );
    }

    #[test]
    fn bit_shift_left() {
        let r = parse("1 << 2").unwrap();
        assert_eq!(
            r,
            Expression::BinaryExpression(
                Op::LShift,
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2))
            )
        );
    }
}
