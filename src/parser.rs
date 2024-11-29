use crate::{
    allocator::{boxed::Box, clone_in::CloneIn, vec::Vec, Allocator},
    error::{Error, Result},
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub(crate) struct Parser<'alloc> {
    allocator: &'alloc Allocator,
    lexer: &'alloc mut Lexer<'alloc>,
    peek: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Ast<'alloc> {
    Char(char),
    Empty,
    Plus(Box<'alloc, Ast<'alloc>>),
    Star(Box<'alloc, Ast<'alloc>>),
    Question(Box<'alloc, Ast<'alloc>>),
    Or(Box<'alloc, Ast<'alloc>>, Box<'alloc, Ast<'alloc>>),
    Seq(Vec<'alloc, Ast<'alloc>>),
}

impl<'old_alloc, 'new_alloc> CloneIn<'new_alloc> for Ast<'old_alloc> {
    type Cloned = Ast<'new_alloc>;

    fn clone_in(&self, allocator: &'new_alloc crate::allocator::Allocator) -> Self::Cloned {
        match self {
            Ast::Char(c) => Ast::Char(*c),
            Ast::Empty => Ast::Empty,
            Ast::Plus(node) => Ast::Plus(node.clone_in(allocator)),
            Ast::Star(node) => Ast::Star(node.clone_in(allocator)),
            Ast::Question(node) => Ast::Question(node.clone_in(allocator)),
            Ast::Or(left, right) => Ast::Or(left.clone_in(allocator), right.clone_in(allocator)),
            Ast::Seq(nodes) => Ast::Seq(nodes.clone_in(allocator)),
        }
    }
}

impl<'alloc> Parser<'alloc> {
    pub(crate) fn new(
        allocator: &'alloc Allocator,
        lexer: &'alloc mut Lexer<'alloc>,
    ) -> Parser<'alloc> {
        let peek = lexer.tokenize();
        Parser {
            allocator,
            lexer,
            peek,
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Ast<'alloc>> {
        let ast = self.parse_expr()?;
        if self.peek != Token::Empty {
            return Err(Error::UnexpectedChar(self.peek));
        }
        Ok(ast)
    }

    fn parse_expr(&mut self) -> Result<Ast<'alloc>> {
        let mut ast = if self.peek == Token::RightParen {
            Ast::Empty
        } else {
            self.parse_term()?
        };
        if self.peek == Token::UnionOp {
            self.consume(Token::UnionOp)?;
            let right = self.parse_expr()?;
            ast = Ast::Or(
                Box::new_in(ast, self.allocator),
                Box::new_in(right, self.allocator),
            );
        }
        Ok(ast)
    }

    fn parse_term(&mut self) -> Result<Ast<'alloc>> {
        let mut nodes = Vec::new_in(self.allocator);
        while self.peek != Token::RightParen
            && self.peek != Token::UnionOp
            && self.peek != Token::Empty
        {
            nodes.push(self.parse_factor()?);
        }
        if nodes.len() == 1 {
            Ok(nodes.pop().unwrap())
        } else {
            Ok(Ast::Seq(nodes))
        }
    }

    fn parse_factor(&mut self) -> Result<Ast<'alloc>> {
        let mut ast = self.parse_atom()?;
        match self.peek {
            Token::PlusOp => {
                self.consume(Token::PlusOp)?;
                ast = Ast::Plus(Box::new_in(ast, self.allocator));
            }
            Token::StarOp => {
                self.consume(Token::StarOp)?;
                ast = Ast::Star(Box::new_in(ast, self.allocator));
            }
            Token::QuestionOp => {
                self.consume(Token::QuestionOp)?;
                ast = Ast::Question(Box::new_in(ast, self.allocator));
            }
            _ => {}
        }
        Ok(ast)
    }

    fn parse_atom(&mut self) -> Result<Ast<'alloc>> {
        match self.peek {
            Token::Char(c) => {
                self.consume(Token::Char(c))?;

                Ok(Ast::Char(c))
            }
            Token::LeftParen => {
                self.consume(Token::LeftParen)?;
                let node = self.parse_expr()?;
                self.consume(Token::RightParen)?;

                Ok(node)
            }
            _ => Err(Error::UnexpectedChar(self.peek)),
        }
    }

    fn consume(&mut self, token: Token) -> Result<()> {
        match &self.peek {
            look if look == &token => {
                self.peek = self.lexer.tokenize();
                Ok(())
            }
            _ => Err(Error::Expected(token)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::{
        allocator::{boxed::Box, Allocator},
        lexer::Lexer,
    };

    #[test]
    fn test_parser_union() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(Ast::Char('b'), &allocator)
            )
        );
    }

    #[test]
    fn test_parser_union_star() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b*", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Star(Box::new_in(Ast::Char('b'), &allocator)),
                    &allocator
                )
            )
        );
    }

    #[test]
    fn test_parser_union_plus() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b+", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Plus(Box::new_in(Ast::Char('b'), &allocator)),
                    &allocator
                )
            )
        );
    }

    #[test]
    fn test_parser_union_question() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b?", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Question(Box::new_in(Ast::Char('b'), &allocator)),
                    &allocator
                )
            )
        );
    }

    #[test]
    fn test_parser_union_union() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b|c", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Or(
                        Box::new_in(Ast::Char('b'), &allocator),
                        Box::new_in(Ast::Char('c'), &allocator)
                    ),
                    &allocator
                )
            )
        );
    }

    #[test]
    fn test_parser_parentheses() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|(b|c)", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Or(
                        Box::new_in(Ast::Char('b'), &allocator),
                        Box::new_in(Ast::Char('c'), &allocator)
                    ),
                    &allocator
                )
            )
        );
    }

    #[test]
    fn test_parser_parentheses_plus_star() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("((a|b)+)*", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Star(Box::new_in(
                Ast::Plus(Box::new_in(
                    Ast::Or(
                        Box::new_in(Ast::Char('a'), &allocator),
                        Box::new_in(Ast::Char('b'), &allocator)
                    ),
                    &allocator
                )),
                &allocator
            ))
        );
    }

    #[test]
    fn test_parser_star_question() {
        let allocator = Allocator::default();
        let mut lexer = Lexer::new("a|b*|c?", &allocator);
        let mut parser = Parser::new(&allocator, &mut lexer);

        assert_eq!(
            parser.parse().unwrap(),
            Ast::Or(
                Box::new_in(Ast::Char('a'), &allocator),
                Box::new_in(
                    Ast::Or(
                        Box::new_in(
                            Ast::Star(Box::new_in(Ast::Char('b'), &allocator)),
                            &allocator
                        ),
                        Box::new_in(
                            Ast::Question(Box::new_in(Ast::Char('c'), &allocator)),
                            &allocator
                        )
                    ),
                    &allocator
                )
            )
        );
    }
}
