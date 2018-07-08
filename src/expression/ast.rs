#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    LogicalOr,
    LogicalAnd,
    BinOr,
    BinAnd,
    BinXor,
    NotEqual,
    Equal,
    LessEqual,
    Less,
    Greater,
    GreaterEqual,
    LShift,
    RShift,
    Add,
    Sub,
    Mod,
    Div,
    Mul,
    LogicalNot,
    BinNot,
    // Unary
    Minus,
    // Unary
    Plus,
}

impl Op {
    pub fn parse<S: AsRef<str>>(s: S) -> Result<Self, String> {
        let op = match s.as_ref() {
            "||" => Op::LogicalOr,
            "&&" => Op::LogicalAnd,
            "|" => Op::BinOr,
            "&" => Op::BinAnd,
            "^" => Op::BinXor,
            "!=" => Op::NotEqual,
            "==" => Op::Equal,
            "<=" => Op::LessEqual,
            "<" => Op::Less,
            ">" => Op::Greater,
            ">=" => Op::GreaterEqual,
            "<<" => Op::LShift,
            ">>" => Op::RShift,
            "+" => Op::Add,
            "-" => Op::Sub,
            "%" => Op::Mod,
            "/" => Op::Div,
            "*" => Op::Mul,
            "!" => Op::LogicalNot,
            "~" => Op::BinNot,
            other => {
                return Err(format!("Unknown operator: {}", other));
            }
        };
        Ok(op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Assignment(Op, String, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    BinaryExpression(Op, Box<Expression>, Box<Expression>),
    UnaryExpression(Op, Box<Expression>),
    Num(i64),
}
