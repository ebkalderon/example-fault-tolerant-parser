use std::cell::RefCell;
use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::{take, take_till1, take_while};
use nom::character::complete::{anychar, char};
use nom::combinator::{all_consuming, map, not, recognize, rest, verify};
use nom::sequence::{delimited, preceded, terminated};

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

impl<'a> ToRange for LocatedSpan<'a> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

#[derive(Debug)]
struct Error(Range<usize>, String);

#[derive(Clone, Debug)]
struct State<'a>(&'a RefCell<Vec<Error>>);

impl<'a> State<'a> {
    pub fn report_error(&self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

fn expect<'a, F, E, T>(parser: F, error_msg: E) -> impl Fn(LocatedSpan<'a>) -> IResult<Option<T>>
where
    F: Fn(LocatedSpan<'a>) -> IResult<T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error((input, _))) | Err(nom::Err::Failure((input, _))) => {
            let err = Error(input.to_range(), error_msg.to_string());
            input.extra.report_error(err);
            Ok((input, None))
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
    let ident = recognize(preceded(first, rest));
    map(ident, |span: LocatedSpan| {
        Expr::Ident(Ident(span.fragment().to_string()))
    })(input)
}

fn paren(input: LocatedSpan) -> IResult<Expr> {
    let paren = delimited(
        char('('),
        expect(expr, "expected expression after `(`"),
        expect(char(')'), "missing `)`"),
    );

    map(paren, |inner| {
        Expr::Paren(Box::new(inner.unwrap_or(Expr::Error)))
    })(input)
}

fn error(input: LocatedSpan) -> IResult<Expr> {
    map(take_till1(|c| c == ')'), |span: LocatedSpan| {
        let err = Error(span.to_range(), format!("unexpected `{}`", span.fragment()));
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
    for input in &["foo", "(foo)", "(foo))", "(%", "(", "%", "()", ""] {
        println!("{:7} {:?}", input, parse(input));
    }
}
