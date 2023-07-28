use crate::{
    ast::{Ast, AstKind},
    CompilerResult
};
use std::{borrow::Cow, cmp::Eq, error::Error, fmt, iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Ident,
    Number(u64),
    Str(Cow<'a, str>),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    SemiColon,
    Colon,
    Function,
    Return,
}

impl<'a> TokenKind<'a> {
    fn prefix_bp(&self) -> Option<usize> {
        match self {
            Self::Sub => Some(5),
            _ => None,
        }
    }

    fn infix_bp(&self) -> Option<(usize, usize)> {
        match self {
            Self::Add | Self::Sub => Some((1, 2)),
            Self::Mul | Self::Div | Self::Mod => Some((3, 4)),
            _ => None,
        }
    }

    fn is_node(&self) -> bool {
        matches!(self, |Self::Ident| Self::Number(_) | Self::Str(_))
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind<'a>,
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TokenKind::Ident => write!(f, "{}", self.text),
            TokenKind::Number(n) => write!(f, "{n}"),
            TokenKind::Str(ref string) => write!(f, "{string:?}"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LCurly => write!(f, "{{"),
            TokenKind::RCurly => write!(f, "}}"),
            TokenKind::LSquare => write!(f, "["),
            TokenKind::RSquare => write!(f, "]"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone)]
pub enum ParseError<'a> {
    InvalidChar(char),
    UnclosedString,
    UnclosedParen(Token<'a>),
    UnexpectedToken(Option<Token<'a>>),
}

impl<'a> Error for ParseError<'a> {}

impl<'a> fmt::Debug for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidChar(ch) => write!(f, "Unexpected character `{ch}`."),
            Self::UnclosedParen(_) => write!(f, "Unclosed parentheses."),
            Self::UnclosedString => write!(f, "Unclosed string."),
            Self::UnexpectedToken(Some(token)) => write!(f, "Unexpected token `{token:?}`"),
            Self::UnexpectedToken(None) => write!(f, "Unexpected EOF."),
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn parse_string(string: &'_ str) -> Cow<'_, str> {
    let mut value = Cow::Borrowed(string);

    let mut iter = string.char_indices();

    while let Some((pos, ch)) = iter.next() {
        if ch == '\\' {
            let mut string = value.into_owned();
            let _ = string.split_off(pos);

            let Some((_, ch)) = iter.next() else {
                unreachable!();
            };

            let new_ch = match ch {
                'r' => '\r',
                'n' => '\n',
                't' => '\t',
                '0' => '\0',
                other => other,
            };

            string.push(new_ch);

            value = Cow::Owned(string);
            continue;
        }

        if let Cow::Owned(ref mut string) = value {
            string.push(ch);
        }
    }

    value
}

#[derive(Clone)]
pub struct Parser<'a> {
    string: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            chars: string.char_indices().peekable(),
        }
    }

    #[inline]
    fn peeking(&mut self, function: impl Fn(char) -> bool) -> bool {
        self.chars.peek().map_or(false, |&(_, ch)| function(ch))
    }

    #[inline]
    fn advance(&mut self, pos: &mut usize) {
        if let Some((new_pos, _)) = self.chars.next() {
            *pos = new_pos;
        }
    }

    fn read_token(&mut self) -> CompilerResult<'a, Option<Token<'a>>> {
        let token = loop {
            let Some(&(mut pos, ch)) = self.chars.peek() else {
                return Ok(None);
            };

            let start_pos = pos;

            if ch.is_ascii_whitespace() {
                while self.peeking(|ch| ch.is_ascii_whitespace()) {
                    self.advance(&mut pos);
                }

                continue;
            }

            if ch.is_ascii_alphabetic() || ch == '_' {
                while self.peeking(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
                    self.advance(&mut pos);
                }

                let text = &self.string[start_pos..=pos];

                let kind = match text {
                    "fn" => TokenKind::Function,
                    "return" => TokenKind::Return,
                    _ => TokenKind::Ident,
                };

                break Token { text, kind };
            }

            if ch.is_ascii_digit() {
                while self.peeking(|ch| ch.is_ascii_digit()) {
                    self.advance(&mut pos);
                }

                let text = &self.string[start_pos..=pos];

                break Token {
                    text,
                    kind: TokenKind::Number(text.parse().unwrap_or_else(|_| unreachable!())),
                };
            }

            if ch == '"' {
                self.chars.next();

                while !self.peeking(|ch| ch == '"') {
                    if self.peeking(|ch| ch == '\\') {
                        self.advance(&mut pos);
                    }
                    self.advance(&mut pos);
                    if self.chars.peek().is_none() {
                        Err(ParseError::UnclosedString)?;
                    }
                }

                self.advance(&mut pos);
                let text = &self.string[start_pos..=pos];
                break Token {
                    text,
                    kind: TokenKind::Str(parse_string(&text[1..text.len() - 1])),
                };
            }

            self.advance(&mut pos);

            let text = &self.string[start_pos..=pos];

            let kind = match ch {
                '+' => TokenKind::Add,
                '-' => TokenKind::Sub,
                '*' => TokenKind::Mul,
                '/' => TokenKind::Div,
                '%' => TokenKind::Mod,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LCurly,
                '}' => TokenKind::RCurly,
                '[' => TokenKind::LSquare,
                ']' => TokenKind::RSquare,
                ';' => TokenKind::SemiColon,
                ':' => TokenKind::Colon,
                _ => Err(ParseError::InvalidChar(ch))?,
            };

            break Token { text, kind };
        };

        Ok(Some(token))
    }

    fn peek_token(&mut self) -> CompilerResult<'a, Option<Token<'a>>> {
        let prev_self = self.clone(); // Fast, shallow copy
        let token = self.read_token();
        *self = prev_self;
        token
    }

    fn expect(&mut self, token_kind: TokenKind<'a>) -> CompilerResult<'a, Option<Token<'a>>> {
        let token = self.read_token()?;

        if token.as_ref().map_or(false, |tok| tok.kind == token_kind) {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken(token))?
        }
    }

    fn parse_expr_bp(&mut self, min_bp: usize) -> CompilerResult<'a, Ast<'a>> {
        let Some(token) = self.read_token()? else {
            Err(ParseError::UnexpectedToken(None))?
        };

        let mut lhs = if let Some(prefix_right_bp) = token.kind.prefix_bp() {
            Ast::new(AstKind::Prefix {
                oper: token,
                node: Box::new(self.parse_expr_bp(prefix_right_bp)?),
            })?
        } else if token.kind.is_node() {
            Ast::new(AstKind::Atom { token })?
        } else if let TokenKind::LParen = token.kind {
            let inside = self.parse_expr_bp(0)?;

            self.expect(TokenKind::RParen)?;

            inside
        } else {
            Err(ParseError::UnexpectedToken(Some(token)))?
        };

        loop {
            let Some(oper) = self.peek_token()? else { break; };

            let Some((infix_left_bp, infix_right_bp)) = oper.kind.infix_bp() else {
                break;
            };

            if infix_left_bp < min_bp {
                break;
            };

            self.read_token()?;

            lhs = Ast::new(AstKind::Infix {
                oper,
                lhs: Box::new(lhs),
                rhs: Box::new(self.parse_expr_bp(infix_right_bp)?),
            })?;
        }

        Ok(lhs)
    }

    pub fn parse(&mut self) -> CompilerResult<'a, Ast<'a>> {
        let expression = self.parse_expr_bp(0);

        if let Some(token) = self.read_token()? {
            Err(ParseError::UnexpectedToken(Some(token)))?
        };

        expression
    }
}
