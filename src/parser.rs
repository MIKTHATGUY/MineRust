/// Parser module using nom for DataRust syntax
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::ast::*;
use crate::error::CompileError;

/// Skip whitespace and comments
/// Skip whitespace and comments
fn ws(input: &str) -> IResult<&str, ()> {
    let mut current = input;

    loop {
        let start = current;

        // Consume whitespace
        let (rest, _) = multispace0(current)?;
        current = rest;

        // Try to parse a line comment
        if current.starts_with("//") {
            // Skip until newline or end of input
            if let Some(newline_pos) = current.find('\n') {
                current = &current[newline_pos + 1..];
            } else {
                current = "";
            }
            continue; // Loop again to check for more whitespace/comments
        }

        // Try to parse a block comment
        if current.starts_with("/*") {
            if let Some(end_pos) = current.find("*/") {
                current = &current[end_pos + 2..];
                continue; // Loop again to check for more whitespace/comments
            } else {
                // Unclosed block comment
                return Err(nom::Err::Error(nom::error::Error::new(
                    current,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }

        // If no whitespace or comments were consumed, we're done
        if current == start {
            break;
        }
    }

    Ok((current, ()))
}
/// Parse the entire DataRust program
pub fn parse(input: &str) -> Result<Program, CompileError> {
    match program(input) {
        Ok((remaining, prog)) => {
            let (remaining, _) = ws(remaining).unwrap_or((remaining, ()));
            if remaining.trim().is_empty() {
                Ok(prog)
            } else {
                Err(CompileError::ParseError(format!(
                    "Unexpected input after parsing: '{}'",
                    remaining
                )))
            }
        }
        Err(e) => Err(CompileError::ParseError(format!("Parse error: {}", e))),
    }
}

/// Parse a complete program
fn program(input: &str) -> IResult<&str, Program> {
    let (input, _) = ws(input)?;
    let (input, items) = many0(preceded(ws, item))(input)?;
    let (input, _) = ws(input)?;
    Ok((input, Program { items }))
}

/// Parse a top-level item (function, struct, enum, const)
fn item(input: &str) -> IResult<&str, Item> {
    alt((
        map(function, Item::Function),
        map(struct_def, Item::Struct),
        // TODO: Add enum and const parsers
    ))(input)
}

/// Parse a function definition
fn function(input: &str) -> IResult<&str, Function> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = ws(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, params) = delimited(
        char('('),
        separated_list0(delimited(ws, char(','), ws), param),
        char(')'),
    )(input)?;
    let (input, _) = ws(input)?;
    let (input, return_type) = opt(preceded(tuple((tag("->"), ws)), type_parser))(input)?;
    let (input, _) = ws(input)?;

    // Parse body between curly braces
    let (input, body) = delimited(
        char('{'),
        |input| {
            let (input, _) = ws(input)?;
            let (input, exprs) = many0(preceded(ws, expr))(input)?;
            let (input, _) = ws(input)?;

            let body = if exprs.len() == 1 {
                exprs.into_iter().next().unwrap()
            } else {
                Expr::new(ExprKind::Block(exprs))
            };

            Ok((input, body))
        },
        char('}'),
    )(input)?;

    Ok((
        input,
        Function {
            name: name.to_string(),
            params,
            return_type,
            body,
        },
    ))
}

/// Parse a function parameter
fn param(input: &str) -> IResult<&str, Param> {
    let (input, _) = ws(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = ws(input)?;
    let (input, ty) = type_parser(input)?;
    let (input, _) = ws(input)?;

    Ok((
        input,
        Param {
            name: name.to_string(),
            ty,
        },
    ))
}

/// Parse a struct definition
fn struct_def(input: &str) -> IResult<&str, StructDef> {
    let (input, _) = tag("struct")(input)?;
    let (input, _) = ws(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, fields) = delimited(
        char('{'),
        separated_list0(delimited(ws, char(','), ws), field),
        char('}'),
    )(input)?;

    Ok((
        input,
        StructDef {
            name: name.to_string(),
            fields,
        },
    ))
}

/// Parse a struct field
fn field(input: &str) -> IResult<&str, Field> {
    let (input, _) = ws(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = ws(input)?;
    let (input, ty) = type_parser(input)?;
    let (input, _) = ws(input)?;

    Ok((
        input,
        Field {
            name: name.to_string(),
            ty,
        },
    ))
}

/// Parse a type
fn type_parser(input: &str) -> IResult<&str, Type> {
    alt((
        // Fast storage modifier
        map(
            tuple((
                tag("fast"),
                ws,
                type_parser,
                opt(delimited(char('<'), digit1, char('>'))),
            )),
            |(_, _, ty, scale)| {
                let scale_val = scale.map(|s| s.parse().unwrap());
                Type::Fast(Box::new(ty), scale_val)
            },
        ),
        // Primitives
        value(Type::Bool, tag("bool")),
        value(Type::I8, tag("i8")),
        value(Type::I16, tag("i16")),
        value(Type::I32, tag("i32")),
        value(Type::I64, tag("i64")),
        value(Type::U8, tag("u8")),
        value(Type::U16, tag("u16")),
        value(Type::U32, tag("u32")),
        value(Type::U64, tag("u64")),
        value(Type::F32, tag("f32")),
        value(Type::F64, tag("f64")),
        value(Type::Str, tag("str")),
        value(Type::Char, tag("char")),
        // Vec
        map(
            delimited(
                tuple((tag("vec"), ws, char('<'), ws)),
                type_parser,
                tuple((ws, char('>'))),
            ),
            |ty| Type::Vec(Box::new(ty)),
        ),
        // Custom types (struct/enum names)
        map(identifier, |name| Type::Struct(name.to_string())),
    ))(input)
}

/// Parse an expression
fn expr(input: &str) -> IResult<&str, Expr> {
    alt((
        expr_let,
        expr_selector,
        expr_as,
        expr_at,
        expr_asat,
        expr_block,
        expr_if,
        expr_binary,
    ))(input)
}

/// Parse a let expression
fn expr_let(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("let")(input)?;
    let (input, _) = ws(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, ty) = opt(preceded(tuple((char(':'), ws)), type_parser))(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = ws(input)?;
    let (input, value) = expr(input)?;
    let (input, _) = ws(input)?; // Consume trailing comments/whitespace

    Ok((
        input,
        Expr::new(ExprKind::Let {
            name: name.to_string(),
            ty,
            value: Box::new(value),
        }),
    ))
}

/// Parse an 'as' execution context
fn expr_as(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("as")(input)?;
    let (input, _) = ws(input)?;

    // Try to parse a selector first (sel::Target)
    if let Ok((remaining, selector_expr)) = expr_selector(input) {
        // Extract the selector from the expression
        if let ExprKind::Selector(selector) = selector_expr.kind {
            let (input, _) = ws(remaining)?;
            let (input, body) = expr_block(input)?;

            return Ok((
                input,
                Expr::new(ExprKind::As {
                    selector: SelectorOrString::Selector(selector),
                    body: Box::new(body),
                }),
            ));
        }
    }

    // Fall back to old string selector parsing
    let (input, selector) = parse_selector(input)?;
    let (input, _) = ws(input)?;
    let (input, body) = expr_block(input)?;

    Ok((
        input,
        Expr::new(ExprKind::As {
            selector: SelectorOrString::String(selector.to_string()),
            body: Box::new(body),
        }),
    ))
}

/// Parse an 'asat' execution context
fn expr_asat(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("asat")(input)?;
    let (input, _) = ws(input)?;

    // Try to parse a selector first (sel::Target)
    if let Ok((remaining, selector_expr)) = expr_selector(input) {
        // Extract the selector from the expression
        if let ExprKind::Selector(selector) = selector_expr.kind {
            let (input, _) = ws(remaining)?;
            let (input, body) = expr_block(input)?;

            return Ok((
                input,
                Expr::new(ExprKind::AsAt {
                    selector: SelectorOrString::Selector(selector),
                    body: Box::new(body),
                }),
            ));
        }
    }

    // Fall back to old string selector parsing
    let (input, selector) = parse_selector(input)?;
    let (input, _) = ws(input)?;
    let (input, body) = expr_block(input)?;

    Ok((
        input,
        Expr::new(ExprKind::AsAt {
            selector: SelectorOrString::String(selector.to_string()),
            body: Box::new(body),
        }),
    ))
}

/// Parse an 'at' execution context
fn expr_at(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("at")(input)?;
    let (input, _) = ws(input)?;
    let (input, position) = parse_position(input)?;
    let (input, _) = ws(input)?;
    let (input, body) = expr_block(input)?;

    Ok((
        input,
        Expr::new(ExprKind::At {
            position,
            body: Box::new(body),
        }),
    ))
}

/// Parse a Minecraft selector (e.g., @p, @e[type=cow])
fn parse_selector(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        char('@'),
        pair(
            alpha1,
            opt(delimited(char('['), take_while(|c| c != ']'), char(']'))),
        ),
    ))(input)
}

/// Parse a position (relative coordinates, struct reference, etc.)
fn parse_position(input: &str) -> IResult<&str, Position> {
    alt((
        // Relative coordinates like ~ ~ ~, ~10 ~5 ~-3
        map(
            recognize(tuple((
                tag("~"),
                opt(recognize(pair(opt(char('-')), digit1))),
                ws,
                tag("~"),
                opt(recognize(pair(opt(char('-')), digit1))),
                ws,
                tag("~"),
                opt(recognize(pair(opt(char('-')), digit1))),
            ))),
            |s: &str| Position::Relative(s.to_string()),
        ),
        // Struct reference (e.g., Pos { x: 0, y: 64, z: 0 })
        map(identifier, |name: &str| Position::Struct(name.to_string())),
    ))(input)
}

/// Parse a block expression
fn expr_block(input: &str) -> IResult<&str, Expr> {
    let (input, exprs) = delimited(
        char('{'),
        many0(preceded(ws, expr)),
        preceded(ws, char('}')),
    )(input)?;

    Ok((input, Expr::new(ExprKind::Block(exprs))))
}

/// Parse an if expression
fn expr_if(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = ws(input)?;
    let (input, condition) = expr(input)?;
    let (input, _) = ws(input)?;
    let (input, then_branch) = expr(input)?;
    let (input, _) = ws(input)?;
    let (input, else_branch) = opt(preceded(tuple((tag("else"), ws)), expr))(input)?;

    Ok((
        input,
        Expr::new(ExprKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }),
    ))
}

/// Parse a binary operation (arithmetic) or cast
fn expr_binary(input: &str) -> IResult<&str, Expr> {
    // Simplified: parse primary then optional operator + right side
    // TODO: Implement proper precedence climbing
    let (input, _) = ws(input)?; // Consume leading whitespace/comments
    let (input, left) = expr_primary(input)?;
    let (input, _) = ws(input)?; // Changed from multispace0 to ws

    // Check for 'as' keyword (type cast)
    if let Ok((input, _)) = tag::<_, _, nom::error::Error<&str>>("as")(input) {
        let (input, _) = ws(input)?;
        let (input, target_ty) = type_parser(input)?;
        return Ok((
            input,
            Expr::new(ExprKind::Cast {
                expr: Box::new(left),
                target_ty,
            }),
        ));
    }

    // Try to parse operator
    let (input, op) = opt(alt((
        value(BinOp::Add, char('+')),
        value(BinOp::Sub, char('-')),
        value(BinOp::Mul, char('*')),
        value(BinOp::Div, char('/')),
        value(BinOp::Mod, char('%')),
    )))(input)?;

    if let Some(op) = op {
        let (input, _) = ws(input)?; // Changed from multispace0 to ws
        let (input, right) = expr(input)?;
        Ok((
            input,
            Expr::new(ExprKind::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parse a primary expression (literals, variables, calls, etc.)
fn expr_primary(input: &str) -> IResult<&str, Expr> {
    let (input, _) = ws(input)?; // Consume leading whitespace/comments
    alt((
        expr_float_lit, // Must come before int_lit to properly parse floats
        expr_int_lit,
        expr_bool_lit,
        expr_char_lit,
        expr_string_lit,
        expr_call_or_var,
        delimited(char('('), preceded(ws, expr), preceded(ws, char(')'))),
    ))(input)
}

/// Parse integer literal
fn expr_int_lit(input: &str) -> IResult<&str, Expr> {
    let (input, num) = recognize(pair(opt(char('-')), digit1))(input)?;
    let (input, suffix) = opt(alt((
        tag("i8"),
        tag("i16"),
        tag("i32"),
        tag("i64"),
        tag("u8"),
        tag("u16"),
        tag("u32"),
        tag("u64"),
    )))(input)?;

    // Parse value - use i64 for signed, but store as string for large unsigned values
    let value = num.parse::<i64>().unwrap_or_else(|_| {
        // For values too large for i64, just use max i64 value
        // The type checker will validate the range
        i64::MAX
    });

    let ty = suffix.map(|s| match s {
        "i8" => Type::I8,
        "i16" => Type::I16,
        "i32" => Type::I32,
        "i64" => Type::I64,
        "u8" => Type::U8,
        "u16" => Type::U16,
        "u32" => Type::U32,
        "u64" => Type::U64,
        _ => Type::I32,
    });

    Ok((input, Expr::new(ExprKind::IntLit(value, ty))))
}

/// Parse float literal
fn expr_float_lit(input: &str) -> IResult<&str, Expr> {
    let (input, num) = recognize(tuple((opt(char('-')), digit1, char('.'), digit1)))(input)?;
    let (input, suffix) = opt(alt((tag("f32"), tag("f64"))))(input)?;

    let value = num.parse::<f64>().unwrap();
    let ty = suffix.map(|s| match s {
        "f32" => Type::F32,
        "f64" => Type::F64,
        _ => Type::F64,
    });

    Ok((input, Expr::new(ExprKind::FloatLit(value, ty))))
}

/// Parse boolean literal
fn expr_bool_lit(input: &str) -> IResult<&str, Expr> {
    alt((
        value(Expr::new(ExprKind::BoolLit(true)), tag("true")),
        value(Expr::new(ExprKind::BoolLit(false)), tag("false")),
    ))(input)
}

/// Parse string literal
fn expr_string_lit(input: &str) -> IResult<&str, Expr> {
    let (input, s) = delimited(char('"'), take_while(|c| c != '"'), char('"'))(input)?;
    Ok((input, Expr::new(ExprKind::StringLit(s.to_string()))))
}

/// Parse char literal
fn expr_char_lit(input: &str) -> IResult<&str, Expr> {
    let (input, c) = delimited(
        char('\''),
        alt((
            // Escape sequences
            map(
                preceded(
                    char('\\'),
                    alt((
                        value('\\', char('\\')),
                        value('\'', char('\'')),
                        value('"', char('"')),
                        value('\n', char('n')),
                        value('\r', char('r')),
                        value('\t', char('t')),
                    )),
                ),
                |ch| ch,
            ),
            // Regular character
            nom::character::complete::none_of("'"),
        )),
        char('\''),
    )(input)?;
    Ok((input, Expr::new(ExprKind::CharLit(c))))
}

/// Parse function call or variable reference
fn expr_call_or_var(input: &str) -> IResult<&str, Expr> {
    let (input, name) = identifier(input)?;
    let (input, _) = ws(input)?; // Changed from multispace0 to ws

    // Check if it's a function call
    if let Ok((input, args)) = delimited(
        char('('),
        separated_list0(delimited(ws, char(','), ws), expr),
        char(')'),
    )(input)
    {
        Ok((
            input,
            Expr::new(ExprKind::Call {
                name: name.to_string(),
                args,
            }),
        ))
    } else {
        Ok((input, Expr::new(ExprKind::Var(name.to_string()))))
    }
}

/// Parse an identifier
fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

/// Parse a selector expression: sel::Target { filters }
fn expr_selector(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("sel")(input)?;
    let (input, _) = tag("::")(input)?;
    let (input, target) = selector_target(input)?;
    let (input, _) = ws(input)?;
    let (input, filters) = delimited(
        char('{'),
        |input| {
            let (input, _) = ws(input)?;
            let (input, filters) =
                separated_list0(delimited(ws, char(','), ws), selector_filter)(input)?;
            let (input, _) = ws(input)?;
            // Allow optional trailing .. or comma
            let (input, _) = opt(alt((tag(".."), tag(","))))(input)?;
            let (input, _) = ws(input)?;
            Ok((input, filters))
        },
        char('}'),
    )(input)?;

    Ok((
        input,
        Expr::new(ExprKind::Selector(Selector { target, filters })),
    ))
}

/// Parse selector target: AllPlayers | NearestPlayer | RandomPlayer | Self | Entities
fn selector_target(input: &str) -> IResult<&str, SelectorTarget> {
    alt((
        value(SelectorTarget::AllPlayers, tag("AllPlayers")),
        value(SelectorTarget::NearestPlayer, tag("NearestPlayer")),
        value(SelectorTarget::RandomPlayer, tag("RandomPlayer")),
        value(SelectorTarget::SelfTarget, tag("Self")),
        value(SelectorTarget::Entities, tag("Entities")),
    ))(input)
}

/// Parse a selector filter: key = value
fn selector_filter(input: &str) -> IResult<&str, SelectorFilter> {
    let (input, _) = ws(input)?;
    let (input, key) = identifier(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = ws(input)?;

    match key {
        "ty" => {
            let (input, entity_type) = identifier(input)?;
            Ok((input, SelectorFilter::Type(entity_type.to_string())))
        }
        "name" => {
            let (input, name) = delimited(char('"'), take_while(|c| c != '"'), char('"'))(input)?;
            Ok((input, SelectorFilter::Name(name.to_string())))
        }
        "dist" => {
            let (input, range) = parse_range(input)?;
            Ok((input, SelectorFilter::Distance(range)))
        }
        "x" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::X(val)))
        }
        "y" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::Y(val)))
        }
        "z" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::Z(val)))
        }
        "dx" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::DX(val)))
        }
        "dy" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::DY(val)))
        }
        "dz" => {
            let (input, val) = parse_number(input)?;
            Ok((input, SelectorFilter::DZ(val)))
        }
        "pitch" => {
            let (input, range) = parse_range(input)?;
            Ok((input, SelectorFilter::Pitch(range)))
        }
        "yaw" => {
            let (input, range) = parse_range(input)?;
            Ok((input, SelectorFilter::Yaw(range)))
        }
        "limit" => {
            let (input, val) = digit1(input)?;
            let limit = val.parse().unwrap();
            Ok((input, SelectorFilter::Limit(limit)))
        }
        "sort" => {
            let (input, sort_type) = alt((
                value(SortType::Nearest, tag("nearest")),
                value(SortType::Furthest, tag("furthest")),
                value(SortType::Random, tag("random")),
                value(SortType::Arbitrary, tag("arbitrary")),
            ))(input)?;
            Ok((input, SelectorFilter::Sort(sort_type)))
        }
        "pred" => {
            // Parse namespace:path format
            let (input, pred) = recognize(tuple((identifier, char(':'), identifier)))(input)?;
            Ok((input, SelectorFilter::Predicate(pred.to_string())))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Parse a number (integer or float)
fn parse_number(input: &str) -> IResult<&str, f64> {
    let (input, num_str) = recognize(tuple((
        opt(char('-')),
        digit1,
        opt(tuple((char('.'), digit1))),
    )))(input)?;
    let value = num_str.parse().unwrap();
    Ok((input, value))
}

/// Parse a range: 5 | 5..10 | ..10 | 5..
fn parse_range(input: &str) -> IResult<&str, RangeValue> {
    alt((
        // ..10 (up to)
        map(preceded(tag(".."), parse_number), |end| {
            RangeValue::UpTo(end)
        }),
        // 5..10 or 5.. (range or from)
        map(
            tuple((parse_number, tag(".."), opt(parse_number))),
            |(start, _, end)| match end {
                Some(e) => RangeValue::Range(start, e),
                None => RangeValue::From(start),
            },
        ),
        // 5 (exact)
        map(parse_number, |val| RangeValue::Exact(val)),
    ))(input)
}
