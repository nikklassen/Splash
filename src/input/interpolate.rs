use env::UserEnv;
use std::env;
use super::tokenizer::AST;

fn expand_arg(arg: AST, user_env: &UserEnv) -> AST {
    match arg {
        AST::Var(v) => {
            let s = user_env.vars
                .get(&v)
                .map(|value| value.clone())
                .unwrap_or_else(|| {
                    env::var(&v)
                    .unwrap_or(String::new())
                });
            return AST::String(s);
        },
        AST::Quoted(ts) => {
            return AST::Quoted(expand(ts.to_owned(), user_env));
        },
        _ => {},
    };
    arg
}

pub fn expand(items: Vec<AST>, user_env: &UserEnv) -> Vec<AST> {
    items.into_iter().map(|arg| expand_arg(arg, user_env)).collect()
}

#[cfg(test)]
mod tests {
    use env::UserEnv;
    use std::env;
    use super::*;
    use input::tokenizer::AST;

    fn make_test_env() -> UserEnv {
        let mut user_env = UserEnv::new();
        user_env.vars.insert("TEST".to_string(), "value".to_string());
        user_env
    }

    #[test]
    fn expand_env_var() {
        let user_env = make_test_env();
        env::set_var("test_var", "value");
        let mut toks = vec![AST::Var("test_var".to_string())];
        toks = expand(toks, &user_env);
        assert_eq!(toks, vec![AST::String("value".to_string())]);
    }

    #[test]
    fn expand_user_var() {
        let user_env = make_test_env();
        let mut toks = vec![AST::Var("TEST".to_string())];
        toks = expand(toks, &user_env);
        assert_eq!(toks, vec![AST::String("value".to_string())]);
    }

    #[test]
    fn expand_quoted() {
        let user_env = make_test_env();
        let mut toks = vec![AST::Quoted(
            vec![AST::Var("TEST".to_string())])];
        toks = expand(toks, &user_env);
        assert_eq!(toks, vec![AST::Quoted(
                vec![AST::String("value".to_string())])]);
    }
}
