use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, none_of},
    combinator::{eof, opt, recognize, value},
    error::ParseError,
    multi::{many0_count, many1, separated_list1},
    sequence::{delimited, pair},
    IResult, Parser,
};

use crate::grammar::nodes::{SyntaxNode, SyntaxNodeKind};

// TODO: error handling

fn ws<'a, O, E, F>(inner: F) -> impl Parser<&'a str, Output = O, Error = E>
where
    E: ParseError<&'a str>,
    F: Parser<&'a str, Output = O, Error = E>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn parse_rule(input: &str) -> IResult<&str, SyntaxNode> {
    let (input, name) = identifier.parse(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, kind) = choice(input)?;
    let (input, _) = eof(input)?;

    Ok((
        input,
        SyntaxNode {
            kind,
            name: name.to_string(),
        },
    ))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    ws(recognize(pair(alpha1, many0_count(alphanumeric1)))).parse(input)
}

fn name(input: &str) -> IResult<&str, SyntaxNodeKind> {
    let (input, matched) = identifier.parse(input)?;
    Ok((input, SyntaxNodeKind::Name(matched.to_string())))
}

fn choice(input: &str) -> IResult<&str, SyntaxNodeKind> {
    let (input, mut choices) = separated_list1(ws(char('|')), list).parse(input)?;

    if choices.len() == 1 {
        Ok((input, choices.pop().unwrap()))
    } else {
        Ok((input, SyntaxNodeKind::Choice(choices)))
    }
}

fn list(input: &str) -> IResult<&str, SyntaxNodeKind> {
    let (input, mut items) = many1(list_item).parse(input)?;

    if items.len() == 1 {
        Ok((input, items.pop().unwrap()))
    } else {
        Ok((input, SyntaxNodeKind::List(items)))
    }
}

fn list_item(input: &str) -> IResult<&str, SyntaxNodeKind> {
    let (input, node) = alt((group, token_pattern, name)).parse(input)?;
    let (input, modifier) = ws(opt(char('?'))).parse(input)?;

    if modifier.is_none() {
        Ok((input, node))
    } else {
        Ok((input, SyntaxNodeKind::Optional(Box::new(node))))
    }
}

fn group(input: &str) -> IResult<&str, SyntaxNodeKind> {
    delimited(ws(char('(')), choice, ws(char(')'))).parse(input)
}

fn token_pattern(input: &str) -> IResult<&str, SyntaxNodeKind> {
    let base = escaped_transform(
        none_of(r"\'"),
        '\\',
        alt((value(r"\", tag(r"\")), value("'", tag("'")))),
    );

    let (input, matched) = ws(delimited(char('\''), base, char('\''))).parse(input)?;
    Ok((input, SyntaxNodeKind::Pattern(matched.into())))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ok {
        ($fn: ident, $str:expr) => {
            assert!($fn($str).is_ok());
        };
    }

    macro_rules! err {
        ($fn: ident, $str:expr) => {
            assert!($fn($str).is_err());
        };
    }

    mod name {
        use super::*;

        #[test]
        fn valid() {
            ok!(name, "abc");
            ok!(name, "abc123");
            ok!(name, "XYZ");
            ok!(name, " XYZ   ");
        }

        #[test]
        fn invalid() {
            err!(name, "");
            err!(name, "1");
            err!(name, "1abc");
            err!(name, "_b");
        }
    }

    mod pattern {
        use super::*;

        #[test]
        fn valid() {
            ok!(token_pattern, "'a'");
            ok!(token_pattern, "'abc'");
            ok!(token_pattern, r"'a\'b'");
            ok!(token_pattern, r"'a\\b'");
        }

        #[test]
        fn invalid() {
            err!(token_pattern, "");
            err!(token_pattern, "'+");
            err!(token_pattern, r"'a\_'");
            ok!(token_pattern, r"'a b'");
        }
    }


    // TODO: more tests
}
