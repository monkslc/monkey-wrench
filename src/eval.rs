use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lexer::Token;
use crate::parser::{Expression, Ident, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Array(Vec<Object>),
    Boolean(bool),
    BuiltInFn(fn(Vec<Object>) -> Object),
    Error(String),
    Fn(Vec<Ident>, Vec<Statement>),
    Int(isize),
    Null,
    Str(String),
}

impl Object {
    fn less_than(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Boolean(a < b),
            (o, p) => Object::Error(format!("'<' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn greater_than(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Boolean(a > b),
            (o, p) => Object::Error(format!("'<' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn add(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a + b),
            (Object::Str(a), Object::Str(b)) => Object::Str(format!("{}{}", a, b)),
            (o, p) => Object::Error(format!("'+' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn subtract(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a - b),
            (o, p) => Object::Error(format!("'+' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn multiply(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a * b),
            (o, p) => Object::Error(format!("'+' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn divide(&self, b: &Object) -> Object {
        match (self, b) {
            (Object::Int(a), Object::Int(b)) => Object::Int(a / b),
            (o, p) => Object::Error(format!("'+' cannot be applied to {:?} and {:?}", o, p)),
        }
    }

    fn truthy(&self) -> bool {
        match self {
            Object::Int(val) if *val != 0 => true,
            Object::Boolean(true) => true,
            Object::Str(val) if val.len() != 0 => true,
            _ => false,
        }
    }
}

pub struct Eval(Rc<RefCell<Environment>>);

impl From<Environment> for Eval {
    fn from(env: Environment) -> Self {
        Eval(Rc::new(RefCell::new(env)))
    }
}

impl Eval {
    pub fn new() -> Self {
        let mut env = Environment::new();
        env.set(String::from("len"), Object::BuiltInFn(built_in_len));
        Eval(Rc::new(RefCell::new(env)))
    }

    fn assign_let(&mut self, ident: String, expr: Expression) {
        let obj = self.eval_expression(expr);
        self.0.borrow_mut().set(ident, obj);
    }

    pub fn eval<I: Iterator<Item = Statement>>(&mut self, statements: I) -> Object {
        let mut result = Object::Null;
        for stmt in statements {
            result = match stmt {
                Statement::Expression(expr) => self.eval_expression(expr),
                Statement::Return(expr) => return self.eval_expression(expr),
                Statement::Let(Ident { value }, expr) => {
                    self.assign_let(value, expr);
                    result
                }
            }
        }
        result
    }

    fn eval_expression(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::Ident(val) => self
                .0
                .borrow()
                .get(&val)
                .map(|val| val.clone())
                .unwrap_or(Object::Null),
            Expression::Int(val) => Object::Int(val),
            Expression::Boolean(val) => Object::Boolean(val),
            Expression::Prefix(token, expr) => match token {
                Token::Bang => Object::Boolean(!self.eval_expression(*expr).truthy()),
                Token::Minus => self.eval_minus_prefix(*expr),
                _ => Object::Null,
            },
            Expression::Infix(left, token, right) => match token {
                Token::Plus => self
                    .eval_expression(*left)
                    .add(&self.eval_expression(*right)),
                Token::Minus => self
                    .eval_expression(*left)
                    .subtract(&self.eval_expression(*right)),
                Token::Asterik => self
                    .eval_expression(*left)
                    .multiply(&self.eval_expression(*right)),
                Token::Slash => self
                    .eval_expression(*left)
                    .divide(&self.eval_expression(*right)),
                Token::Equal => {
                    Object::Boolean(self.eval_expression(*left) == self.eval_expression(*right))
                }
                Token::NotEqual => {
                    Object::Boolean(self.eval_expression(*left) != self.eval_expression(*right))
                }
                Token::LessThan => self
                    .eval_expression(*left)
                    .less_than(&self.eval_expression(*right)),
                Token::GreaterThan => self
                    .eval_expression(*left)
                    .greater_than(&self.eval_expression(*right)),
                _ => Object::Null,
            },
            Expression::If(expr, consequent, alt) => self.eval_if(*expr, consequent, alt),
            Expression::Fn(params, body) => Object::Fn(params, body),
            Expression::Call(expr, args) => self.eval_call_expr(*expr, args),
            Expression::Str(val) => Object::Str(val),
            Expression::Array(elements) => Object::Array(
                elements
                    .into_iter()
                    .map(|e| self.eval_expression(e))
                    .collect(),
            ),
        }
    }

    fn eval_minus_prefix(&mut self, expr: Expression) -> Object {
        match self.eval_expression(expr) {
            Object::Int(val) => Object::Int(-val),
            o => Object::Error(format!("'-' cannot be applied to {:?}", o)),
        }
    }

    fn eval_if(
        &mut self,
        expr: Expression,
        consequent: Vec<Statement>,
        alt: Vec<Statement>,
    ) -> Object {
        if self.eval_expression(expr).truthy() {
            Eval::from(Environment::from(Rc::clone(&self.0))).eval(consequent.into_iter())
        } else {
            Eval::from(Environment::from(Rc::clone(&self.0))).eval(alt.into_iter())
        }
    }

    fn eval_call_expr(&mut self, expr: Expression, args: Vec<Expression>) -> Object {
        let outer = Rc::clone(&self.0);
        let outer_env = outer.borrow();
        match expr {
            Expression::Ident(id) => match outer_env.get(&id) {
                Some(Object::Fn(params, body)) => {
                    let mut env = Environment::from(Rc::clone(&outer));
                    args.into_iter().zip(params.iter()).for_each(|(expr, arg)| {
                        env.set(arg.value.clone(), self.eval_expression(expr))
                    });
                    Eval::from(env).eval(body.into_iter())
                }
                Some(Object::BuiltInFn(func)) => {
                    let args = args
                        .into_iter()
                        .map(|arg| self.eval_expression(arg))
                        .collect();
                    func(args)
                }
                _ => Object::Null,
            },
            e => match self.eval_expression(e) {
                Object::Fn(params, body) => {
                    let mut env = Environment::from(Rc::clone(&outer));
                    args.into_iter().zip(params.iter()).for_each(|(expr, arg)| {
                        env.set(arg.value.clone(), self.eval_expression(expr))
                    });
                    Eval::from(env).eval(body.into_iter())
                }
                _ => Object::Error(String::from("Your IIFE seems to have failed")),
            },
        }
    }
}

pub struct Environment {
    pub store: HashMap<String, Object>,
    pub parent: Option<Rc<RefCell<Environment>>>,
}

impl From<Rc<RefCell<Environment>>> for Environment {
    fn from(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            parent: Some(parent),
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }

    fn get(&self, var: &str) -> Option<Object> {
        match self.store.get(var) {
            Some(var) => Some(var.clone()),
            None => match self.parent {
                Some(ref parent) => parent.borrow().get(var),
                None => None,
            },
        }
    }

    fn set(&mut self, var: String, val: Object) {
        self.store.insert(var, val);
    }
}

fn built_in_len(args: Vec<Object>) -> Object {
    match args.get(0) {
        Some(Object::Str(val)) => Object::Int(val.len() as isize),
        Some(t) => Object::Error(format!("Invalid argument to len: {:?}", t)),
        None => Object::Error(format!("Pass an arg to len")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_literals() {
        let input = r#"5; false; true;"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![
            Object::Int(5),
            Object::Boolean(false),
            Object::Boolean(true),
        ];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_prefix_bang() {
        let input = r#"!false; !5; !0"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
        ];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_prefix_minus() {
        let input = r#"-5; -false;"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![
            Object::Int(-5),
            Object::Error(String::from("'-' cannot be applied to Boolean(false)")),
        ];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_infix_math() {
        let input = r#"5 + 5; false + 3; 5 - 3;"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![
            Object::Int(10),
            Object::Error(String::from(
                "'+' cannot be applied to Boolean(false) and Int(3)",
            )),
            Object::Int(2),
        ];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_infix_boolean() {
        let input = r#"5 < 6; 5 == 4; true < false;"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Error(String::from(
                "'<' cannot be applied to Boolean(true) and Boolean(false)",
            )),
        ];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_if_expression() {
        let input = r#"if 5 < 6 { !5; }; if 5 < 3 { 5; } else { 3; };"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![Object::Boolean(false), Object::Int(3)];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_return_statement() {
        let input = r#"return 5; if 5 { return 5; 9; }"#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let results: Vec<Object> = statements
            .unwrap()
            .into_iter()
            .map(|stmt| Eval::new().eval(vec![stmt].into_iter()))
            .collect();

        let expected = vec![Object::Int(5), Object::Int(5)];

        assert_eq!(results, expected);
    }

    #[test]
    fn test_assign() {
        let input = r#"
          let x = 5;
          let y = x;
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let mut eval = Eval::new();
        eval.eval(statements.unwrap().into_iter());

        let env = eval.0.borrow();
        let actual = vec![env.get(&String::from("x")), env.get(&String::from("y"))];
        let expected = vec![Some(Object::Int(5)), Some(Object::Int(5))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_environment() {
        let input = r#"
          let x = 5;
          if x {
            let x = 6;
          }

          return x;
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();
        let statements = statements.unwrap();
        let result = Eval::new().eval(statements.into_iter());

        let expected = Object::Int(5);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_environment_2() {
        let input = r#"
          let x = 5;
          if x {
            x + 10
          }
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();
        let statements = statements.unwrap();
        let result = Eval::new().eval(statements.into_iter());

        let expected = Object::Int(15);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_function_obj() {
        let input = r#"
            let add = fn x, y {
                x + y
            };
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();
        let statements = statements.unwrap();
        let mut eval = Eval::new();
        eval.eval(statements.into_iter());

        let env = eval.0.borrow();
        let var = env.get("add");
        let expected = Object::Fn(
            vec![Ident::new(String::from("x")), Ident::new(String::from("y"))],
            vec![Statement::Expression(Expression::Infix(
                Box::new(Expression::Ident(String::from("x"))),
                Token::Plus,
                Box::new(Expression::Ident(String::from("y"))),
            ))],
        );
        assert_eq!(var, Some(expected));
    }

    #[test]
    fn test_call_expr() {
        let input = r#"
            let add = fn x, y {
                x + y
            };

            fn x, y { x + y }(add(3, 2), 2);
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let result = Eval::new().eval(statements.unwrap().into_iter());
        let expected = Object::Int(7);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_strings() {
        let input = r#"
            if 'Hi' {
              'hello' + ' ' + 'world'
            }
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let result = Eval::new().eval(statements.unwrap().into_iter());
        let expected = Object::Str(String::from("hello world"));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_len() {
        let input = r#"
            len('12345');
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let result = Eval::new().eval(statements.unwrap().into_iter());
        let expected = Object::Int(5);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_array() {
        let input = r#"
            [1, 'hey'];
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Result<Vec<Statement>, String> = parser.statements().collect();

        let result = Eval::new().eval(statements.unwrap().into_iter());
        let expected = Object::Array(vec![Object::Int(1), Object::Str(String::from("hey"))]);
        assert_eq!(result, expected);
    }
}
