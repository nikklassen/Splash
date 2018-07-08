use super::ast::*;
use env::UserEnv;

pub fn eval(e: &Expression, user_env: &mut UserEnv) -> i64 {
    match e {
        Expression::UnaryExpression(op, e) => eval_unary(*op, e, user_env),
        Expression::BinaryExpression(op, lhs, rhs) => eval_binary(*op, lhs, rhs, user_env),
        Expression::Assignment(op, s, e) => eval_assignment(*op, s, e, user_env),
        Expression::Conditional(c, t, f) => eval_conditional(c, t, f, user_env),
        Expression::Num(n) => *n,
    }
}

fn eval_unary(op: Op, e: &Expression, user_env: &mut UserEnv) -> i64 {
    let r = eval(e, user_env);
    match op {
        Op::Plus => r,
        Op::Minus => r * -1,
        Op::LogicalNot => {
            if r == 0 {
                1
            } else {
                0
            }
        }
        Op::BinNot => !r,
        _ => unreachable!(),
    }
}

fn eval_binary(op: Op, lhs: &Expression, rhs: &Expression, user_env: &mut UserEnv) -> i64 {
    let lhs_v = eval(lhs, user_env);
    match op {
        Op::LogicalOr => {
            return if lhs_v == 0 {
                eval(rhs, user_env)
            } else {
                lhs_v
            };
        }
        Op::LogicalAnd => {
            return if lhs_v != 0 {
                eval(rhs, user_env)
            } else {
                lhs_v
            };
        }
        _ => {}
    }
    let rhs_v = eval(rhs, user_env);
    match op {
        Op::BinOr => lhs_v | rhs_v,
        Op::BinAnd => lhs_v & rhs_v,
        Op::BinXor => lhs_v ^ rhs_v,
        Op::NotEqual => (lhs_v != rhs_v) as i64,
        Op::Equal => (lhs_v == rhs_v) as i64,
        Op::LessEqual => (lhs_v <= rhs_v) as i64,
        Op::Less => (lhs_v < rhs_v) as i64,
        Op::Greater => (lhs_v > rhs_v) as i64,
        Op::GreaterEqual => (lhs_v >= rhs_v) as i64,
        Op::LShift => lhs_v << rhs_v,
        Op::RShift => lhs_v >> rhs_v,
        Op::Add => lhs_v + rhs_v,
        Op::Sub => lhs_v - rhs_v,
        Op::Mod => lhs_v % rhs_v,
        Op::Div => lhs_v / rhs_v,
        Op::Mul => lhs_v * rhs_v,
        _ => unreachable!(),
    }
}

fn eval_assignment(op: Op, target: &str, e: &Expression, user_env: &mut UserEnv) -> i64 {
    let r = if op == Op::Equal {
        eval(e, user_env)
    } else {
        let prev_value = user_env.get(target).parse::<i64>().unwrap_or(0);
        eval_binary(op, &Expression::Num(prev_value), e, user_env)
    };
    user_env.set(target, format!("{}", r));
    r
}

fn eval_conditional(
    condition: &Expression,
    true_branch: &Expression,
    false_branch: &Expression,
    user_env: &mut UserEnv,
) -> i64 {
    let r = eval(condition, user_env);
    if r == 0 {
        eval(false_branch, user_env)
    } else {
        eval(true_branch, user_env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add() {
        let e = Expression::BinaryExpression(
            Op::Add,
            Box::new(Expression::Num(1)),
            Box::new(Expression::Num(1)),
        );
        let mut user_env = UserEnv::new();
        let r = eval(&e, &mut user_env);
        assert_eq!(r, 2);
    }

    #[test]
    fn unary_minus() {
        let e = Expression::UnaryExpression(Op::Minus, Box::new(Expression::Num(1)));
        let mut user_env = UserEnv::new();
        let r = eval(&e, &mut user_env);
        assert_eq!(r, -1);
    }

    #[test]
    fn chained_assignment() {
        let e = Expression::Assignment(
            Op::Equal,
            String::from("a"),
            Box::new(Expression::Assignment(
                Op::Mul,
                String::from("b"),
                Box::new(Expression::Num(2)),
            )),
        );
        let mut user_env = UserEnv::new();
        user_env.set("b", "2");
        let r = eval(&e, &mut user_env);
        assert_eq!(r, 4);
        assert_eq!(user_env.get("a"), "4");
        assert_eq!(user_env.get("b"), "4");
    }

    #[test]
    fn ternary_true() {
        let e = Expression::Conditional(
            Box::new(Expression::Num(1)),
            Box::new(Expression::Num(2)),
            Box::new(Expression::Num(3)),
        );
        let mut user_env = UserEnv::new();
        let r = eval(&e, &mut user_env);
        assert_eq!(r, 2);
    }

    #[test]
    fn ternary_false() {
        let e = Expression::Conditional(
            Box::new(Expression::Num(0)),
            Box::new(Expression::Num(2)),
            Box::new(Expression::Num(3)),
        );
        let mut user_env = UserEnv::new();
        let r = eval(&e, &mut user_env);
        assert_eq!(r, 3);
    }
}
