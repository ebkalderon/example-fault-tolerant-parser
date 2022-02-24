use std::cell::RefCell;
use std::ops::{Range};

use nom::branch::alt;
use nom::bytes::complete::{take, take_till1, take_while};
use nom::character::complete::{anychar, char, multispace0};
use nom::combinator::{all_consuming, map, not, recognize, rest, verify};
use nom::sequence::{delimited, preceded, terminated};

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

#[derive(Debug)]
struct PosInfo(u32, Range<usize>);
trait Position {
    fn to_position(&self) -> PosInfo;
}

impl<'a> Position for LocatedSpan<'a> {
    fn to_position(&self) -> PosInfo {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        PosInfo {
            0: self.location_line(),
            1: start..end,
        }
    }
}

#[derive(Debug)]
struct Error(PosInfo, String);

#[derive(Clone, Debug)]
struct State<'a>(&'a RefCell<Vec<Error>>);

impl<'a> State<'a> {
    pub fn report_error(&self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

fn ws<'a, F, T>(parser: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<T>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    delimited(multispace0, parser, multispace0)
}

fn expect<'a, F, E, T>(
    mut parser: F,
    error_msg: E,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<Option<T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(nom::error::Error { input: i, .. }))
        | Err(nom::Err::Failure(nom::error::Error { input: i, .. })) => {
            let err = Error(i.to_position(), error_msg.to_string());
            i.extra.report_error(err);
            Ok((i, None))
        }
        Err(err) => Err(err),
    }
}

#[derive(Debug)]
struct Ident(String);

#[derive(Debug)]
enum Expr {
    Ident(Ident),
    Paren(Box<Expr>),
    Error,
}

fn ident(input: LocatedSpan) -> IResult<Expr> {
    let first = verify(anychar, |c| c.is_ascii_alphabetic() || *c == '_');
    let rest = take_while(|c: char| c.is_ascii_alphanumeric() || "_-'".contains(c));
    let ident2 = recognize(preceded(first, rest));
    map(ws(ident2), |span: LocatedSpan| {
        Expr::Ident(Ident(span.fragment().to_string()))
    })(input)
}

fn paren(input: LocatedSpan) -> IResult<Expr> {
    let paren = delimited(
        ws(char('(')),
        expect(ws(expr), "expected expression after `(`"),
        expect(ws(char(')')), "missing `)`"),
    );

    map(paren, |inner| {
        Expr::Paren(Box::new(inner.unwrap_or(Expr::Error)))
    })(input)
}

fn error(input: LocatedSpan) -> IResult<Expr> {
    map(take_till1(|c| c == ')'), |span: LocatedSpan| {
        let err = Error(
            span.to_position(),
            format!("unexpected `{}`", span.fragment()),
        );
        span.extra.report_error(err);
        Expr::Error
    })(input)
}

fn expr(input: LocatedSpan) -> IResult<Expr> {
    alt((paren, ident, error))(input)
}

fn source_file(input: LocatedSpan) -> IResult<Expr> {
    let expr = alt((expr, map(take(0usize), |_| Expr::Error)));
    terminated(expr, preceded(expect(not(anychar), "expected EOF"), rest))(input)
}

fn parse(source: &str) -> (Expr, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, expr) = all_consuming(source_file)(input).expect("parser cannot fail");
    (expr, errors.into_inner())
}

fn main() {
    for input in &[
        "foo ",
        "(foo)",
        "(foo))",
        "\n\n(foo\n\n)",
        "      (           foo      ) \n       ",
        "(%",
        "(%)",
        "(",
        "%",
        "()",
        "",
    ] {
        println!("{:7} {:?}", input, parse(input));
    }
}
