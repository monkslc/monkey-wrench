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
    True,
}

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
}
