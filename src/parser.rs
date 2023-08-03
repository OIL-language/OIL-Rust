use crate::{
    ast::{Ast, AstKind},
    types::{DataType, IntType, InferredType},
    symbol_table::{SymbolTable, Variable},
    CompilerResult,
};
use std::{borrow::Cow, cmp::Eq, error::Error, fmt, iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'src> {
    Ident,
    Number(u64),
    Str(Cow<'src, str>),
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
    Equals,
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl<'src> TokenKind<'src> {
    fn prefix_bp(&self) -> Option<usize> {
        match self {
            Self::Sub => Some(5),
            _ => None
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
        matches!(self, Self::Ident | Self::Number(_) | Self::Str(_) | Self::True | Self::False)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Token<'src> {
    pub text: &'src str,
    pub kind: TokenKind<'src>,
}

impl<'src> fmt::Debug for Token<'src> {
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
            TokenKind::Equals => write!(f, "="),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
        }
    }
}

#[derive(Clone)]
pub enum ParseError<'src> {
    InvalidChar(char),
    UnclosedString,
    UnclosedParen(Token<'src>),
    UnexpectedToken(Option<Token<'src>>),
}

impl<'src> Error for ParseError<'src> {}

impl<'src> fmt::Debug for ParseError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidChar(ch) => write!(f, "Unexpected character `{ch}`."),
            Self::UnclosedString => write!(f, "Unclosed string."),
            Self::UnclosedParen(_) => write!(f, "Unclosed parentheses."),
            Self::UnexpectedToken(Some(token)) => write!(f, "Unexpected token `{token:?}`"),
            Self::UnexpectedToken(None) => write!(f, "Unexpected EOF."),
        }
    }
}

impl<'src> fmt::Display for ParseError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn parse_string(string: &'_ str) -> Cow<'_, str> {
    let mut value = Cow::Borrowed(string);

    let mut iter = string.char_indices();

    while let Some((pos, ch)) = iter.next() {
        if ch != '\\' {
            if let Cow::Owned(ref mut string) = value {
                string.push(ch);
            }
            continue;
        }

        let mut string = value.into_owned();
        let _ = string.split_off(pos);

        let Some((_, ch)) = iter.next() else {
            unreachable!("String should've been checked beforehand.");
        };

        string.push(match ch {
            'r' => '\r',
            'n' => '\n',
            't' => '\t',
            '0' => '\0',
            other => other,
        });

        value = Cow::Owned(string);
    }

    value
}

#[derive(Clone)]
pub struct Parser<'src> {
    string: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(string: &'src str) -> Self {
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

    fn read_token(&mut self) -> CompilerResult<'src, Option<Token<'src>>> {
        while let Some(&(mut pos @ start_pos, ch)) = self.chars.peek() {
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

                return Ok(Some(Token {
                    text,
                    kind: match text {
                        "fn" => TokenKind::Function,
                        "let" => TokenKind::Let,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "return" => TokenKind::Return,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        _ => TokenKind::Ident,
                    },
                }));
            }

            if ch.is_ascii_digit() {
                while self.peeking(|ch| ch.is_ascii_digit()) {
                    self.advance(&mut pos);
                }

                let text = &self.string[start_pos..=pos];

                return Ok(Some(Token {
                    text,
                    kind: TokenKind::Number(unsafe { text.parse().unwrap_unchecked() }), // We deliberately unwrap here
                }));
            }

            if ch == '"' {
                self.advance(&mut pos);

                while !self.peeking(|ch| ch == '"') {
                    if self.peeking(|ch| ch == '\\') {
                        self.advance(&mut pos);
                    }

                    self.advance(&mut pos);

                    if self.chars.peek().is_none() {
                        return Err(ParseError::UnclosedString.into());
                    }
                }

                self.advance(&mut pos);

                let text = &self.string[start_pos..=pos];

                return Ok(Some(Token {
                    text,
                    kind: TokenKind::Str(parse_string(&text[1..text.len() - 1])),
                }));
            }

            self.advance(&mut pos);

            let text = &self.string[start_pos..=pos];

            return Ok(Some(Token {
                text,
                kind: match ch {
                    '+' => TokenKind::Add,
                    '-' => TokenKind::Sub,
                    '*' => TokenKind::Mul,
                    '/' => TokenKind::Div,
                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    '{' => TokenKind::LCurly,
                    '}' => TokenKind::RCurly,
                    '[' => TokenKind::LSquare,
                    ']' => TokenKind::RSquare,
                    ';' => TokenKind::SemiColon,
                    '=' => TokenKind::Equals,
                    ':' => TokenKind::Colon,
                    other => return Err(ParseError::InvalidChar(other).into()),
                },
            }));
        }

        Ok(None)
    }

    fn peek_token(&mut self) -> CompilerResult<'src, Option<Token<'src>>> {
        let prev_self = self.clone(); // Fast, shallow copy
        let token = self.read_token();
        *self = prev_self;
        token
    }

    fn expect(&mut self, kind: TokenKind<'src>) -> CompilerResult<'src, Token<'src>> {
        let token = self.read_token()?
            .ok_or(ParseError::UnexpectedToken(None))?;

        if token.kind == kind {
            Ok(token)
        } else {
            return Err(ParseError::UnexpectedToken(Some(token)).into());
        }
    }

    fn parse_statement(&mut self, symbol_table: &mut SymbolTable<'src>) -> CompilerResult<'src, Ast<'src>> {
        if let Some(Token { kind: TokenKind::Let, .. }) = self.peek_token()? {
            self.read_token()?;

            let name = self.expect(TokenKind::Ident)?;

            let data_type = if let Some(Token { kind: TokenKind::Colon, .. }) = self.peek_token()? {
                self.read_token()?;

                self.parse_data_type()?
            } else {
                DataType::Inferred(InferredType::Any)
            };

            let value = if let Some(Token { kind: TokenKind::Equals, .. }) = self.peek_token()? {
                self.read_token()?;
                Some(Box::new(self.parse_expr_bp(symbol_table, 0)?))
            } else {
                None
            };

            symbol_table.add_variable(name.text, Variable { data_type: data_type.clone() });

            return Ast::new(symbol_table, AstKind::Declaration { name: name.text, data_type, value });
        }

        let lhs = self.parse_expr_bp(symbol_table, 0)?;

        let Some(Token { kind: TokenKind::Equals, .. }) = self.peek_token()? else {
            return Ok(lhs);
        };

        self.read_token()?;

        let rhs = self.parse_expr_bp(symbol_table, 0)?;

        Ast::new(symbol_table, AstKind::Assign { lhs: Box::new(lhs), rhs: Box::new(rhs) })
    }

    fn parse_block(&mut self, symbol_table: &mut SymbolTable<'src>) -> CompilerResult<'src, Ast<'src>> {
        let scope_id = symbol_table.add_scope();

        let mut statements: Vec<Ast<'src>> = Vec::new();

        self.read_token()?;

        loop {
            if let Some(Token { kind: TokenKind::RCurly, .. }) = self.peek_token()? {
                break;
            }

            let statement = self.parse_statement(symbol_table)?;

            statements.push(statement);

            let Some(peek) = self.peek_token()? else {
                return Err(ParseError::UnexpectedToken(None).into());
            };

            match peek.kind {
                TokenKind::RCurly => break,
                TokenKind::SemiColon => self.read_token()?,
                _ => return Err(ParseError::UnexpectedToken(Some(peek)).into())
            };
        }

        self.read_token()?;

        symbol_table.leave_scope();

        Ast::new(symbol_table, AstKind::Block { statements, scope_id })
    }

    fn parse_data_type(&mut self) -> CompilerResult<'src, DataType> {
        let Some(token) = self.read_token()? else {
            return Err(ParseError::UnexpectedToken(None).into());
        };

        let data_type = if token.kind == TokenKind::Ident {
            match token.text {
                "Void"   => DataType::Void,
                "Bool"   => DataType::Bool,
                "S8"     => DataType::Int(IntType::S8),
                "S16"    => DataType::Int(IntType::S16),
                "S32"    => DataType::Int(IntType::S32),
                "S64"    => DataType::Int(IntType::S64),
                "U8"     => DataType::Int(IntType::U8),
                "U16"    => DataType::Int(IntType::U16),
                "U32"    => DataType::Int(IntType::U32),
                "U64"    => DataType::Int(IntType::U64),
                "String" => DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                _        => return Err(ParseError::UnexpectedToken(Some(token)).into())
            }
        } else {
            return Err(ParseError::UnexpectedToken(Some(token)).into());
        };

        Ok(data_type)
    }

    fn parse_expr_bp(&mut self, symbol_table: &mut SymbolTable<'src>, min_bp: usize) -> CompilerResult<'src, Ast<'src>> {
        let token = self.peek_token()?.ok_or(ParseError::UnexpectedToken(None))?;

        let mut lhs = match &token.kind {
            TokenKind::LParen => {
                self.read_token()?;

                let inside = self.parse_expr_bp(symbol_table, 0)?;

                self.expect(TokenKind::RParen)?;

                inside
            },
            TokenKind::LCurly => self.parse_block(symbol_table)?,
            TokenKind::If => {
                self.read_token()?;
                let condition = self.parse_expr_bp(symbol_table, 0)?;

                let if_block = self.parse_block(symbol_table)?;

                let else_block = if let Some(Token { kind: TokenKind::Else, .. }) = self.peek_token()? {
                    self.read_token()?;

                    Some(Box::new(self.parse_block(symbol_table)?))
                } else {
                    None
                };

                Ast::new(
                    symbol_table,
                    AstKind::IfStatement {
                        condition: Box::new(condition),
                        if_block: Box::new(if_block),
                        else_block
                    }
                )?
            },
            kind if kind.is_node() => {
                self.read_token()?;

                Ast::new(symbol_table, AstKind::Node { token })?
            },
            other => {
                self.read_token()?;

                let Some(prefix_bp) = other.prefix_bp() else {
                    return Err(ParseError::UnexpectedToken(Some(token)).into());
                };

                let node = self.parse_expr_bp(symbol_table, prefix_bp)?;

                Ast::new(
                    symbol_table,
                    AstKind::Prefix {
                        oper: token,
                        node: Box::new(node),
                    }
                )?
            }
        };

        loop {
            let Some(oper) = self.peek_token()? else {
                break;
            };

            let Some((infix_left_bp, infix_right_bp)) = oper.kind.infix_bp() else {
                break;
            };

            if infix_left_bp < min_bp {
                break;
            };

            self.read_token()?;

            let rhs = self.parse_expr_bp(symbol_table, infix_right_bp)?;

            lhs = Ast::new(
                symbol_table,
                AstKind::Infix {
                    oper,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            )?;
        }

        Ok(lhs)
    }

    pub fn parse(&mut self, symbol_table: &mut SymbolTable<'src>) -> CompilerResult<'src, Ast<'src>> {
        let expression = self.parse_expr_bp(symbol_table, 0)?;

        if let Some(token) = self.read_token()? {
            return Err(ParseError::UnexpectedToken(Some(token)).into());
        };

        Ok(expression)
    }
}
