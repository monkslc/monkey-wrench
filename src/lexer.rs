use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "let" => Token::Let,
    "fn" => Token::Fn,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Assign,
    Asterik,
    Bang,
    Comma,
    Else,
    Equal,
    NotEqual,
    False,
    Fn,
    GreaterThan,
    Ident(String),
    If,
    Illegal,
    Int(String),
    LeftBrace,
    LeftParen,
    LessThan,
    Let,
    Minus,
    Plus,
    Return,
    RightBrace,
    RightParen,
    Semi,
    Slash,
    Str(String),
    True,
}

impl Token {
    pub fn variant(&self) -> String {
        match self {
            Token::Assign => String::from("Assign"),
            Token::Asterik => String::from("Asterik"),
            Token::Bang => String::from("Bang"),
            Token::Comma => String::from("Comma"),
            Token::Else => String::from("Else"),
            Token::Equal => String::from("Equal"),
            Token::NotEqual => String::from("NotEqual"),
            Token::False => String::from("False"),
            Token::Fn => String::from("Fn"),
            Token::GreaterThan => String::from("GreaterThan"),
            Token::Ident(_) => String::from("Ident"),
            Token::If => String::from("If"),
            Token::Illegal => String::from("Illegal"),
            Token::Int(_) => String::from("Int"),
            Token::LeftBrace => String::from("LeftBrace"),
            Token::LeftParen => String::from("LeftParen"),
            Token::LessThan => String::from("LessThan"),
            Token::Let => String::from("Let"),
            Token::Minus => String::from("Minus"),
            Token::Plus => String::from("Plus"),
            Token::Return => String::from("Return"),
            Token::RightBrace => String::from("RightBrace"),
            Token::RightParen => String::from("RightParen"),
            Token::Semi => String::from("Semi"),
            Token::Slash => String::from("Slash"),
            Token::Str(_) => String::from("String"),
            Token::True => String::from("True"),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    chars: Vec<char>,
}

impl Lexer {
    pub fn new(content: &str) -> Lexer {
        Lexer {
            chars: content.chars().collect(),
        }
    }

    pub fn tokens(&self) -> Tokens {
        Tokens {
            chars: &self.chars,
            current_index: 0,
        }
    }
}

#[derive(Debug)]
pub struct Tokens<'a> {
    chars: &'a Vec<char>,
    current_index: usize,
}

impl<'a> Tokens<'a> {
    pub fn take_char(&mut self) -> Option<char> {
        match self.chars.get(self.current_index) {
            Some(ch) => {
                self.current_index += 1;
                Some(*ch)
            }
            None => None,
        }
    }

    pub fn take_char_if<F>(&mut self, take: &F) -> Option<char>
    where
        F: Fn(char) -> bool,
    {
        match self.peek_char() {
            Some(ch) => {
                if take(ch) {
                    self.current_index += 1;
                    Some(ch)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.chars.get(self.current_index).map(|ch| *ch)
    }

    fn eat_whitespace(&mut self) {
        while let Some(' ') | Some('\t') | Some('\n') | Some('\r') = self.peek_char() {
            self.current_index += 1;
        }
    }

    fn take_while<F>(&mut self, start_char: char, take: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut token = start_char.to_string();
        while let Some(ch) = self.take_char_if(&take) {
            token.push(ch)
        }
        token
    }

    fn next_token(&mut self) -> Option<Token> {
        self.eat_whitespace();
        let token = match self.take_char() {
            Some('(') => Some(Token::LeftParen),
            Some(')') => Some(Token::RightParen),
            Some('*') => Some(Token::Asterik),
            Some('+') => Some(Token::Plus),
            Some(',') => Some(Token::Comma),
            Some('-') => Some(Token::Minus),
            Some('/') => Some(Token::Slash),
            Some(';') => Some(Token::Semi),
            Some('<') => Some(Token::LessThan),
            Some('>') => Some(Token::GreaterThan),
            Some('{') => Some(Token::LeftBrace),
            Some('}') => Some(Token::RightBrace),
            Some('=') => {
                if let Some('=') = self.take_char_if(&|ch| ch == '=') {
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }
            Some('!') => {
                if let Some('=') = self.take_char_if(&|ch| ch == '=') {
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Bang)
                }
            }
            Some(ch @ 'a'..='z') | Some(ch @ '_') => {
                let token = self.take_while(ch, |ch| match ch {
                    'a'..='z' | '_' => true,
                    _ => false,
                });

                match KEYWORDS.get(&token[..]) {
                    Some(keyword) => Some(keyword.clone()),
                    None => Some(Token::Ident(token)),
                }
            }
            Some(ch @ '0'..='9') => {
                let token = self.take_while(ch, |ch| match ch {
                    '0'..='9' => true,
                    _ => false,
                });
                Some(Token::Int(token))
            }
            Some('\'') => match self.take_char() {
                Some('\'') => Some(Token::Str(String::new())),
                Some(ch) => {
                    let token = Some(Token::Str(String::from(
                        self.take_while(ch, |ch| ch != '\''),
                    )));
                    self.take_char();
                    token
                }
                None => Some(Token::Illegal),
            },
            Some(_) => Some(Token::Illegal),
            _ => None,
        };
        token
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_lex_parens() {
        let tokens: Vec<Token> = Lexer::new("(())#").tokens().collect();
        let expected = vec![LeftParen, LeftParen, RightParen, RightParen, Illegal];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_lex_ident() {
        let tokens: Vec<Token> = Lexer::new("foo(bar_foo").tokens().collect();
        let expected = vec![
            Ident(String::from("foo")),
            LeftParen,
            Ident(String::from("bar_foo")),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_lex_keyword() {
        let tokens: Vec<Token> = Lexer::new("let foo letbar").tokens().collect();
        let expected = vec![
            Let,
            Ident(String::from("foo")),
            Ident(String::from("letbar")),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_lex_int() {
        let tokens: Vec<Token> = Lexer::new("let 1234a").tokens().collect();
        let expected = vec![Let, Int(String::from("1234")), Ident(String::from("a"))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_lex_two_char_tokens() {
        let tokens: Vec<Token> = Lexer::new("3 != 3 == !4").tokens().collect();
        let expected = vec![
            Int(String::from("3")),
            NotEqual,
            Int(String::from("3")),
            Equal,
            Bang,
            Int(String::from("4")),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_string() {
        let tokens: Vec<Token> = Lexer::new("'Hello World'").tokens().collect();
        let expected = vec![Str(String::from("Hello World"))];
        assert_eq!(tokens, expected);
    }
}
