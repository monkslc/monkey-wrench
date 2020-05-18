use std::{cmp::Ordering, collections::BTreeMap, collections::HashMap};

use crate::lexer::{Lexer, Token, Tokens};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Array(Vec<Expression>),
    Boolean(bool),
    Call(Box<Expression>, Vec<Expression>),
    Fn(Vec<Ident>, Vec<Statement>),
    Ident(String),
    If(Box<Expression>, Vec<Statement>, Vec<Statement>),
    Index(Box<Expression>, Box<Expression>),
    Infix(Box<Expression>, Token, Box<Expression>),
    Int(isize),
    Map(Vec<(Expression, Expression)>),
    Prefix(Token, Box<Expression>),
    Str(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub value: String,
}

impl Ident {
    pub fn new(value: String) -> Self {
        Ident { value }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum OperatorPrecedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl PartialOrd for OperatorPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OperatorPrecedence {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as usize).cmp(&(*other as usize))
    }
}

fn get_token_precedence(token: &Token) -> OperatorPrecedence {
    return match token {
        Token::Equal => OperatorPrecedence::Equals,
        Token::NotEqual => OperatorPrecedence::Equals,
        Token::LessThan => OperatorPrecedence::LessGreater,
        Token::GreaterThan => OperatorPrecedence::LessGreater,
        Token::Plus => OperatorPrecedence::Sum,
        Token::Minus => OperatorPrecedence::Sum,
        Token::Slash => OperatorPrecedence::Product,
        Token::Asterik => OperatorPrecedence::Product,
        Token::LeftParen => OperatorPrecedence::Call,
        _ => OperatorPrecedence::Lowest,
    };
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    pub fn statements(&self) -> Statements {
        Statements {
            lexer: self.lexer.tokens().peekable(),
        }
    }
}

#[derive(Debug)]
pub struct Statements<'a> {
    lexer: std::iter::Peekable<Tokens<'a>>,
}

impl<'a> Statements<'a> {
    fn parse_let(&mut self) -> Result<Statement, String> {
        match self.expect_token("Ident") {
            Ok(Token::Ident(id)) => match self.expect_token("Assign") {
                Ok(_) => match self.lexer.next() {
                    Some(token) => match self.parse_expression(token, OperatorPrecedence::Lowest) {
                        Ok(expr) => {
                            if let Some(Token::Semi) = self.lexer.peek() {
                                self.lexer.next().unwrap();
                            };
                            Ok(Statement::Let(Ident::new(id), expr))
                        }
                        Err(e) => Err(e),
                    },
                    None => Err(String::from("Oops, lexer ran dry")),
                },
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
            _ => panic!("should never get here"),
        }
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        match self.lexer.next() {
            Some(token) => match self.parse_expression(token, OperatorPrecedence::Lowest) {
                Ok(expr) => {
                    if let Some(Token::Semi) = self.lexer.peek() {
                        self.lexer.next().unwrap();
                    };
                    Ok(Statement::Return(expr))
                }
                Err(e) => Err(e),
            },
            None => Err(String::from("Oh uh the lexer ran out")),
        }
    }

    fn parse_expression_statement(&mut self, first_token: Token) -> Result<Statement, String> {
        match self.parse_expression(first_token, OperatorPrecedence::Lowest) {
            Ok(expr) => {
                if let Some(Token::Semi) = self.lexer.peek() {
                    self.lexer.next().unwrap();
                };
                Ok(Statement::Expression(expr))
            }
            Err(e) => Err(e),
        }
    }

    fn parse_expression(
        &mut self,
        token: Token,
        precedence: OperatorPrecedence,
    ) -> Result<Expression, String> {
        let left_expression = match token {
            Token::Ident(val) => Ok(Expression::Ident(val)),
            Token::Int(val) => {
                if let Ok(val) = val.parse::<isize>() {
                    Ok(Expression::Int(val))
                } else {
                    Err(format!(
                        "Ooops, looks like that wasn't a valid integer: {}",
                        val
                    ))
                }
            }
            Token::Bang | Token::Minus => self.parse_prefix(token),
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            Token::LeftParen => self.parse_grouped_expression(),
            Token::LeftSq => self.parse_array().map(|elem| Expression::Array(elem)),
            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_fn_expression(),
            Token::Str(val) => Ok(Expression::Str(val)),
            Token::LeftBrace => self.parse_map(),
            _ => Err(format!(
                "Token: {:?} lead you here, but I can't parse that",
                token
            )),
        };

        match left_expression {
            Ok(expr) => self.recursive_infix_parse(precedence, expr),
            Err(e) => Err(e),
        }
    }

    fn parse_prefix(&mut self, token: Token) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(next_token) => match self.parse_expression(next_token, OperatorPrecedence::Prefix)
            {
                Ok(expr) => Ok(Expression::Prefix(token, Box::new(expr))),
                Err(e) => Err(e),
            },
            None => Err(String::from("Oops missing token for prefix function")),
        }
    }

    fn parse_infix(&mut self, token: Token, left: Expression) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(next_token) => {
                let precedence = get_token_precedence(&token);
                match self.parse_expression(next_token, precedence) {
                    Ok(right) => Ok(Expression::Infix(Box::new(left), token, Box::new(right))),
                    Err(e) => Err(e),
                }
            }
            None => Err(format!(
                "Ooooops looks like you ran outta room: {:?} {:?}",
                left, token
            )),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(next_token) => {
                let exp = self.parse_expression(next_token, OperatorPrecedence::Lowest);
                match self.lexer.peek() {
                    Some(Token::RightParen) => {
                        self.lexer.next().unwrap();
                        exp
                    }
                    _ => Err(String::from("Hey we missed something")),
                }
            }
            None => Err(String::from("Uhhhh, you need something here")),
        }
    }

    fn parse_array(&mut self) -> Result<Vec<Expression>, String> {
        let mut elements: Vec<Expression> = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Token::Comma) => (),
                Some(Token::RightSq) => break Ok(elements),
                Some(t) => match self.parse_expression(t, OperatorPrecedence::Lowest) {
                    Ok(expr) => elements.push(expr),
                    Err(e) => break Err(e),
                },
                _ => break Err(String::from("Invalid eoi")),
            };
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(token) => match self.parse_expression(token, OperatorPrecedence::Lowest) {
                Ok(condition) => match self.lexer.next() {
                    Some(Token::LeftBrace) => match self.parse_block_statement() {
                        Ok(consequence) => {
                            let alternative = match self.lexer.peek() {
                                Some(Token::Else) => {
                                    self.lexer.next().unwrap();
                                    match self.lexer.next() {
                                        Some(Token::LeftBrace) => self.parse_block_statement(),
                                        _ => Err(String::from("A left brace must follow an else")),
                                    }
                                }
                                _ => Ok(vec![]),
                            };
                            alternative
                                .map(|alt| Expression::If(Box::new(condition), consequence, alt))
                        }
                        Err(e) => Err(e),
                    },
                    Some(token) => Err(format!("Expected Left Brace but found: {:?}", token)),
                    None => Err(String::from(
                        "I ran out of things to parse after the if statement",
                    )),
                },
                Err(e) => Err(e),
            },
            None => Err(String::from("You can't parse an if without an expression")),
        }
    }

    fn parse_fn_expression(&mut self) -> Result<Expression, String> {
        match self.parse_fn_params() {
            Ok(params) => match self.parse_block_statement() {
                Ok(stmt) => Ok(Expression::Fn(params, stmt)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Ident>, String> {
        let mut params: Vec<Ident> = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Token::Ident(t)) => params.push(Ident::new(t)),
                Some(Token::Comma) => (),
                Some(Token::LeftBrace) => break Ok(params),
                Some(token) => {
                    break Err(format!(
                        "Unexpected Token: {:?} when looking for params",
                        token
                    ))
                }
                _ => break Err(String::from("Invalid eoi")),
            };
        }
    }

    fn parse_block_statement(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        loop {
            if let Some(Token::RightBrace) = self.lexer.peek() {
                self.lexer.next().unwrap();
                break Ok(statements);
            }
            match self.next() {
                Some(Ok(stmt)) => statements.push(stmt),
                Some(Err(e)) => break Err(e),
                None => break Err(String::from("Unexpected eoi on dat block statement")),
            }
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, String> {
        match self.parse_call_args() {
            Ok(args) => Ok(Expression::Call(Box::new(left), args)),
            Err(e) => Err(e),
        }
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expression>, String> {
        let mut args: Vec<Expression> = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Token::Comma) => (),
                Some(Token::RightParen) => break Ok(args),
                Some(t) => match self.parse_expression(t, OperatorPrecedence::Lowest) {
                    Ok(expr) => args.push(expr),
                    Err(e) => break Err(e),
                },
                _ => break Err(String::from("Invalid eoi")),
            };
        }
    }

    fn parse_map(&mut self) -> Result<Expression, String> {
        let mut pairs = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Token::Comma) => (),
                Some(Token::RightBrace) => break Ok(Expression::Map(pairs)),
                Some(t) => match self.parse_expression(t, OperatorPrecedence::Lowest) {
                    Ok(key) => match self.lexer.next() {
                        Some(Token::Colon) => match self.lexer.next() {
                            Some(token) => {
                                match self.parse_expression(token, OperatorPrecedence::Lowest) {
                                    Ok(value) => pairs.push((key, value)),
                                    Err(e) => break Err(e),
                                }
                            }
                            None => break Err(String::from("Expected another token bet got eoi")),
                        },
                        _ => {
                            break Err(String::from("Expected a : for the map but didn't get that"))
                        }
                    },
                    Err(e) => break Err(e),
                },
                _ => break Err(String::from("Invalid eoi")),
            };
        }
    }

    fn recursive_infix_parse(
        &mut self,
        precedence: OperatorPrecedence,
        expr: Expression,
    ) -> Result<Expression, String> {
        match self.lexer.peek() {
            Some(next_token) => {
                match next_token {
                    Token::Plus
                    | Token::Minus
                    | Token::Slash
                    | Token::Asterik
                    | Token::Equal
                    | Token::NotEqual
                    | Token::LessThan
                    | Token::GreaterThan
                        if get_token_precedence(next_token) > precedence =>
                    {
                        let next_token = self.lexer.next().unwrap();
                        match self.parse_infix(next_token, expr) {
                            Ok(new_expr) => self.recursive_infix_parse(precedence, new_expr),
                            Err(e) => Err(e),
                        }
                    }
                    Token::LeftSq => {
                        self.lexer.next().unwrap();
                        match self.lexer.next() {
                            Some(token) => {
                                match self.parse_expression(token, OperatorPrecedence::Lowest) {
                                Ok(idx_expr) => match self.lexer.peek() {
                                    Some(Token::RightSq) => {
                                        self.lexer.next().unwrap();
                                        Ok(Expression::Index(Box::new(expr), Box::new(idx_expr)))
                                    }
                                    Some(t) => Err(format!("{:?} is not a valid end for your index", t)),
                                    _ => Err(String::from("End of file was reached for an infix index operator???"))
                                },
                                Err(e) => Err(e),
                            }
                            }
                            None => Err(String::from(
                                "That doesn't look like an index function to me",
                            )),
                        }
                    }
                    Token::LeftParen => {
                        self.lexer.next().unwrap();
                        match self.parse_call_expression(expr) {
                            Ok(new_expr) => self.recursive_infix_parse(precedence, new_expr),
                            Err(e) => Err(e),
                        }
                    }
                    _ => Ok(expr),
                }
            }
            None => Ok(expr),
        }
    }

    fn expect_token(&mut self, expected_variant: &str) -> Result<Token, String> {
        if let Some(token) = self.lexer.next() {
            if token.variant() == expected_variant {
                Ok(token)
            } else {
                Err(format!(
                    "Invalid token. Expected: {}, Received: {:?}",
                    expected_variant, token
                ))
            }
        } else {
            Err(format!("Invalid EOF. Expected: {}", expected_variant))
        }
    }
}

impl<'a> Iterator for Statements<'a> {
    type Item = Result<Statement, String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Token::Let) => Some(self.parse_let()),
            Some(Token::Return) => Some(self.parse_return()),
            None => None,
            Some(token) => Some(self.parse_expression_statement(token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let input = r#"
            let x = 5;
            let y = 8675309;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Let(
                Ident::new(String::from("x")),
                Expression::Int(5),
            )),
            Ok(Statement::Let(
                Ident::new(String::from("y")),
                Expression::Int(8675309),
            )),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_parse_return_statement() {
        let input = r#"
            return 5;
            return 8675309;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Return(Expression::Int(5))),
            Ok(Statement::Return(Expression::Int(8675309))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_parse_expression_statement() {
        let input = r#"
            x;
            5;
            !1;
            -x;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Expression(Expression::Ident(String::from("x")))),
            Ok(Statement::Expression(Expression::Int(5))),
            Ok(Statement::Expression(Expression::Prefix(
                Token::Bang,
                Box::new(Expression::Int(1)),
            ))),
            Ok(Statement::Expression(Expression::Prefix(
                Token::Minus,
                Box::new(Expression::Ident(String::from("x"))),
            ))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_parse_infix_expression_statement() {
        let input = r#"
            5 + 5;
            5 - x;
            5 - !x;
            1 + 2 * 5;
            -1 + 2;
            1 + 2 + 3;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(5)),
                Token::Plus,
                Box::new(Expression::Int(5)),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(5)),
                Token::Minus,
                Box::new(Expression::Ident(String::from("x"))),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(5)),
                Token::Minus,
                Box::new(Expression::Prefix(
                    Token::Bang,
                    Box::new(Expression::Ident(String::from("x"))),
                )),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(1)),
                Token::Plus,
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(2)),
                    Token::Asterik,
                    Box::new(Expression::Int(5)),
                )),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Prefix(
                    Token::Minus,
                    Box::new(Expression::Int(1)),
                )),
                Token::Plus,
                Box::new(Expression::Int(2)),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(1)),
                    Token::Plus,
                    Box::new(Expression::Int(2)),
                )),
                Token::Plus,
                Box::new(Expression::Int(3)),
            ))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_parse_boolean() {
        let input = r#"
            let x = true;
            false;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Let(
                Ident::new(String::from("x")),
                Expression::Boolean(true),
            )),
            Ok(Statement::Expression(Expression::Boolean(false))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_parse_grouped_expression() {
        let input = r#"
            (1 + 2);
            (5 + 5) * 2;
            (5 + (5 - 2)) * 3;
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(1)),
                Token::Plus,
                Box::new(Expression::Int(2)),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Plus,
                    Box::new(Expression::Int(5)),
                )),
                Token::Asterik,
                Box::new(Expression::Int(2)),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Plus,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Int(5)),
                        Token::Minus,
                        Box::new(Expression::Int(2)),
                    )),
                )),
                Token::Asterik,
                Box::new(Expression::Int(3)),
            ))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_if_expr() {
        let input = r#"
            if x < y { x } else { y };
            if 5 { 5 }
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Expression(Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::Ident(String::from("x"))),
                    Token::LessThan,
                    Box::new(Expression::Ident(String::from("y"))),
                )),
                vec![Statement::Expression(Expression::Ident(String::from("x")))],
                vec![Statement::Expression(Expression::Ident(String::from("y")))],
            ))),
            Ok(Statement::Expression(Expression::If(
                Box::new(Expression::Int(5)),
                vec![Statement::Expression(Expression::Int(5))],
                vec![],
            ))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_fn_expr() {
        let input = r#"
        fn x, y { x + y };
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![Ok(Statement::Expression(Expression::Fn(
            vec![Ident::new(String::from("x")), Ident::new(String::from("y"))],
            vec![Statement::Expression(Expression::Infix(
                Box::new(Expression::Ident(String::from("x"))),
                Token::Plus,
                Box::new(Expression::Ident(String::from("y"))),
            ))],
        )))];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_fn_call() {
        let input = r#"
            add (2, 3);
            add(5, 2) * 100
            5 * add((5+2))
        "#;
        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![
            Ok(Statement::Expression(Expression::Call(
                Box::new(Expression::Ident(String::from("add"))),
                vec![Expression::Int(2), Expression::Int(3)],
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Call(
                    Box::new(Expression::Ident(String::from("add"))),
                    vec![Expression::Int(5), Expression::Int(2)],
                )),
                Token::Asterik,
                Box::new(Expression::Int(100)),
            ))),
            Ok(Statement::Expression(Expression::Infix(
                Box::new(Expression::Int(5)),
                Token::Asterik,
                Box::new(Expression::Call(
                    Box::new(Expression::Ident(String::from("add"))),
                    vec![Expression::Infix(
                        Box::new(Expression::Int(5)),
                        Token::Plus,
                        Box::new(Expression::Int(2)),
                    )],
                )),
            ))),
        ];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_string() {
        let input = r#"
            let x = 'Hello';
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![Ok(Statement::Let(
            Ident::new(String::from("x")),
            Expression::Str(String::from("Hello")),
        ))];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_array() {
        let input = r#"
            let x = [1, x];
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![Ok(Statement::Let(
            Ident::new(String::from("x")),
            Expression::Array(vec![
                Expression::Int(1),
                Expression::Ident(String::from("x")),
            ]),
        ))];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_index() {
        let input = r#"
            x[1];
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![Ok(Statement::Expression(Expression::Index(
            Box::new(Expression::Ident(String::from("x"))),
            Box::new(Expression::Int(1)),
        )))];

        assert_eq!(statements, expected);
    }

    #[test]
    fn test_map() {
        let input = r#"
            {'one': 1, 2: 'two'}
        "#;

        let parser = Parser::new(Lexer::new(input));
        let statements: Vec<Result<Statement, String>> = parser.statements().collect();

        let expected = vec![Ok(Statement::Expression(Expression::Map(vec![
            (Expression::Str(String::from("one")), Expression::Int(1)),
            (Expression::Int(2), Expression::Str(String::from("two"))),
        ])))];

        assert_eq!(statements, expected);
    }
}
