use std::{fmt, str::Chars};

use crate::allocator::{boxed::Box, Allocator};

#[derive(Debug)]
pub(crate) struct Lexer<'a> {
    input: Box<'a, Chars<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Char(char),
    Empty,
    LeftParen,
    RightParen,
    UnionOp,
    StarOp,
    PlusOp,
    QuestionOp,
}

impl Lexer<'_> {
    pub(crate) fn new<'a>(input: &'a str, allocator: &'a Allocator) -> Lexer<'a> {
        Lexer {
            input: Box::new_in(input.chars(), allocator),
        }
    }

    pub(crate) fn tokenize(&mut self) -> Token {
        if let Some(c) = self.input.next() {
            match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '|' => Token::UnionOp,
                '*' => Token::StarOp,
                '+' => Token::PlusOp,
                '?' => Token::QuestionOp,
                _ => Token::Char(c),
            }
        } else {
            Token::Empty
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "{}", c),
            Token::UnionOp => write!(f, "|"),
            Token::StarOp => write!(f, "*"),
            Token::PlusOp => write!(f, "+"),
            Token::QuestionOp => write!(f, "?"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Empty => write!(f, "[empty]"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::allocator::Allocator;

    #[test]
    fn test_lexer_union() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new("a|b", &arena);
        assert_eq!(lexer.tokenize(), Token::Char('a'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::Char('b'));
        assert_eq!(lexer.tokenize(), Token::Empty);
    }

    #[test]
    fn test_lexer_union_star() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new("a|b*", &arena);
        assert_eq!(lexer.tokenize(), Token::Char('a'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::Char('b'));
        assert_eq!(lexer.tokenize(), Token::StarOp);
        assert_eq!(lexer.tokenize(), Token::Empty);
    }

    #[test]
    fn test_lexer_union_plus() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new("a|b+", &arena);
        assert_eq!(lexer.tokenize(), Token::Char('a'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::Char('b'));
        assert_eq!(lexer.tokenize(), Token::PlusOp);
        assert_eq!(lexer.tokenize(), Token::Empty);
    }

    #[test]
    fn test_lexer_union_question() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new("a|b?", &arena);
        assert_eq!(lexer.tokenize(), Token::Char('a'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::Char('b'));
        assert_eq!(lexer.tokenize(), Token::QuestionOp);
        assert_eq!(lexer.tokenize(), Token::Empty);
    }

    #[test]
    fn test_lexer_parentheses() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new("a|(b|c)", &arena);
        assert_eq!(lexer.tokenize(), Token::Char('a'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::LeftParen);
        assert_eq!(lexer.tokenize(), Token::Char('b'));
        assert_eq!(lexer.tokenize(), Token::UnionOp);
        assert_eq!(lexer.tokenize(), Token::Char('c'));
        assert_eq!(lexer.tokenize(), Token::RightParen);
        assert_eq!(lexer.tokenize(), Token::Empty);
    }

    #[test]
    fn test_lexer_empty() {
        let arena = Allocator::default();
        let mut lexer = Lexer::new(r#""#, &arena);
        assert_eq!(lexer.tokenize(), Token::Empty);
    }
}
