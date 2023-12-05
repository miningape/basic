use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::HashMap,
    env,
    fmt::Debug,
    fs::{self, File},
    io::{BufRead, BufReader, Lines},
    iter::Peekable,
    rc::Rc,
    str::Chars,
};

use rand::{rngs::ThreadRng, thread_rng, Rng};
use text_io::read;

#[derive(Debug, Clone, PartialEq)]
enum Literal {
    Number(f32),
    String(String),
}

impl Literal {
    fn negate(self) -> Option<Literal> {
        match self {
            Literal::Number(num) => Some(Literal::Number(-num)),
            Literal::String(_) => None,
        }
    }

    fn to_string(self) -> String {
        match &self {
            Literal::Number(num) => format!("{}", num),
            Literal::String(str) => str.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Keyword {
    Data,
    Def,
    Fn,
    Dim,
    End,
    For,
    To,
    Step,
    GoSub,
    GoTo,
    If,
    Then,
    Input,
    Let,
    Next,
    On,
    Print,
    Randomise,
    Read,
    Rem,
    Restore,
    Return,
    Stop,
    And,
    Or,
    Not,
}

impl Keyword {
    fn from(source: String) -> Option<Keyword> {
        Some(match source.to_uppercase().as_str() {
            "DATA" => Keyword::Data,
            "DEF" => Keyword::Def,
            "FN" => Keyword::Fn,
            "DIM" => Keyword::Dim,
            "END" => Keyword::End,
            "FOR" => Keyword::For,
            "TO" => Keyword::To,
            "STEP" => Keyword::Step,
            "GOSUB" => Keyword::GoSub,
            "GOTO" => Keyword::GoTo,
            "IF" => Keyword::If,
            "THEN" => Keyword::Then,
            "INPUT" => Keyword::Input,
            "LET" => Keyword::Let,
            "NEXT" => Keyword::Next,
            "ON" => Keyword::On,
            "PRINT" => Keyword::Print,
            "RANDOMISE" => Keyword::Randomise,
            "READ" => Keyword::Read,
            "REM" => Keyword::Rem,
            "RESTORE" => Keyword::Restore,
            "RETURN" => Keyword::Return,
            "STOP" => Keyword::Stop,
            "AND" => Keyword::And,
            "OR" => Keyword::Or,
            "NOT" => Keyword::Not,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Literal(Literal),
    Identifier(String),
    Keyword(Keyword),

    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    NotEquals,
    Lesser,
    LesserEquals,
    Greater,
    GreaterEquals,
    Carrot,

    SemiColon,
    Colon,

    // Dollar,
    LeftParen,
    RightParen,

    Comma,

    Newline,
    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    line: usize,
    char: usize,
    source: String,
    symbol: Symbol,
}

fn tokenise_number(chars: &mut Peekable<Chars>) -> Result<(String, Symbol, usize), String> {
    let mut source = String::new();

    while let Some(char) = chars.peek() {
        if !char.is_numeric() {
            break;
        }

        source += &chars
            .next()
            .ok_or("Cannot read next char in line")?
            .to_string();
    }

    let literal = source.parse::<i32>().map_err(|err| err.to_string())?;
    Ok((
        source.clone(),
        Symbol::Literal(Literal::Number(literal as f32)),
        source.len(),
    ))
}

fn tokenise_string(chars: &mut Peekable<Chars>) -> Result<(String, Symbol, usize), String> {
    let first = chars.next().ok_or("Cannot read first \" of string")?;
    if first != '"' {
        return Err(String::from("First char in string is not \""));
    }

    let mut source = String::new();
    while let Some(char) = chars.peek() {
        if *char == '"' {
            break;
        }

        let char = chars.next().unwrap();
        source.push(char);
    }

    match chars.next() {
        Some(char) if char == '"' => Ok((
            format!("\"{}\"", source),
            Symbol::Literal(Literal::String(source.clone())),
            source.len() + 2,
        )),
        _ => Err(format!("String (\"{}\") did not end with \"", source)),
    }
}

fn tokenise_identifier(chars: &mut Peekable<Chars>) -> Result<(String, Symbol, usize), String> {
    let first = chars.next().ok_or("Cannot read first \" of string")?;
    if !first.is_alphabetic() {
        return Err(String::from("First char in identifier is not alphabetic"));
    }

    let mut source = String::from(first);
    while let Some(char) = chars.peek() {
        if !(char.is_alphanumeric() || char == &'$') {
            break;
        }

        let char = chars.next().unwrap();
        source.push(char);
    }

    Ok((
        source.clone(),
        if let Some(keyword) = Keyword::from(source.clone()) {
            Symbol::Keyword(keyword)
        } else {
            Symbol::Identifier(source.clone())
        },
        source.len(),
    ))
}

fn tokenise_single(
    symbol: Symbol,
    chars: &mut Peekable<Chars>,
) -> Result<(String, Symbol, usize), String> {
    Ok((
        String::from(chars.next().ok_or("Could not get next single character")?),
        symbol,
        1 as usize,
    ))
}

fn tokenise_line(line: String, line_number: usize) -> Result<Vec<Token>, (String, usize)> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut chars = line.chars().peekable();
    let mut offset = 0;
    while let Some(char) = chars.peek() {
        let (source, symbol, token_offset) = match &char {
            '(' => tokenise_single(Symbol::LeftParen, &mut chars).map_err(|str| (str, offset))?,
            ')' => tokenise_single(Symbol::RightParen, &mut chars).map_err(|str| (str, offset))?,
            '+' => tokenise_single(Symbol::Plus, &mut chars).map_err(|str| (str, offset))?,
            '-' => tokenise_single(Symbol::Minus, &mut chars).map_err(|str| (str, offset))?,
            '*' => tokenise_single(Symbol::Star, &mut chars).map_err(|str| (str, offset))?,
            '/' => tokenise_single(Symbol::Slash, &mut chars).map_err(|str| (str, offset))?,
            '>' => {
                chars.next().unwrap();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    (String::from(">="), Symbol::GreaterEquals, 2)
                } else {
                    (String::from(">"), Symbol::Greater, 1)
                }
            }
            '<' => {
                chars.next().unwrap();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    (String::from("<="), Symbol::LesserEquals, 2)
                } else if chars.peek() == Some(&'>') {
                    chars.next();
                    (String::from("<>"), Symbol::NotEquals, 2)
                } else {
                    (String::from("<"), Symbol::Lesser, 1)
                }
            }
            '=' => tokenise_single(Symbol::Equals, &mut chars).map_err(|str| (str, offset))?,
            // '$' => tokenise_single(Symbol::Dollar, &mut chars).map_err(|str| (str, offset))?,
            ':' => tokenise_single(Symbol::Colon, &mut chars).map_err(|str| (str, offset))?,
            ';' => tokenise_single(Symbol::SemiColon, &mut chars).map_err(|str| (str, offset))?,
            ',' => tokenise_single(Symbol::Comma, &mut chars).map_err(|str| (str, offset))?,
            '"' => tokenise_string(&mut chars).map_err(|str| (str, offset))?,
            _ if (&char).is_alphabetic() => {
                tokenise_identifier(&mut chars).map_err(|str| (str, offset))?
            }
            _ if (&char).is_numeric() => {
                tokenise_number(&mut chars).map_err(|str| (str, offset))?
            }
            _ if (&char).is_whitespace() => {
                chars.next();
                offset += 1;
                continue;
            }
            _ => {
                return Err((
                    format!("Could not recognise character: \'{}\'", char),
                    offset,
                ));
            }
        };

        tokens.push(Token {
            line: line_number,
            char: offset,
            source,
            symbol,
        });

        offset += token_offset;
    }

    tokens.push(Token {
        line: line_number,
        char: offset,
        source: String::from("\n"),
        symbol: Symbol::Newline,
    });

    Ok(tokens)
}

fn tokenise(file: Lines<BufReader<File>>) -> Result<Vec<Token>, (String, String, usize, usize)> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut length = 0;

    for (i, line) in file.enumerate() {
        length = i;
        let line = line.map_err(|err| (String::new(), err.to_string(), i, 0))?;
        let line_tokens = tokenise_line(line.clone(), i).map_err(|(err, ch)| (line, err, i, ch))?;
        for token in line_tokens {
            tokens.push(token)
        }
    }

    tokens.push(Token {
        line: length + 1,
        char: 0,
        source: String::new(),
        symbol: Symbol::EOF,
    });

    Ok(tokens)
}

fn file_iterator() -> Lines<BufReader<File>> {
    let arguments: Vec<String> = env::args().collect();
    let filepath = &arguments.get(1).unwrap();
    let file = File::open(filepath).unwrap();
    let reader = BufReader::new(file);

    return reader.lines();
}

trait Expression: Debug {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)>;
    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String>;
}

impl Expression for Literal {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)> {
        Ok(self.clone())
    }

    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String> {
        Err(String::from("Cannot assign literal"))
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    token: Token,
}
impl Expression for Variable {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)> {
        Ok(interpreter
            .get(self.name.clone())
            .unwrap_or(Literal::Number(0.0)))
    }

    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String> {
        interpreter.set(self.name.clone(), value);
        Ok(())
    }
}

#[derive(Debug)]
struct FunctionCall {
    name: String,
    token: Token,
    arguments: Vec<Rc<RefCell<dyn Expression>>>,
}
impl Expression for FunctionCall {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)> {
        if vec!["TAB", "RND", "INT"].contains(&self.name.to_uppercase().as_str()) {
            if self.arguments.len() == 1 {
                if let Literal::Number(argument) = self
                    .arguments
                    .get(0)
                    .unwrap()
                    .borrow()
                    .interpret(interpreter)?
                {
                    match self.name.to_uppercase().as_str() {
                        "TAB" => {
                            return Ok(Literal::String(vec![' '; argument as usize].iter().fold(
                                String::new(),
                                |mut acc, cur| {
                                    acc.push(*cur);
                                    acc
                                },
                            )));
                        }
                        "RND" => {
                            let result: f32 = interpreter.random.gen();
                            return Ok(Literal::Number(result));
                        }
                        "INT" => {
                            return Ok(Literal::Number(argument.trunc()));
                        }
                        _ => (),
                    }
                }
            }
        }

        Err((
            String::from("Function calls not implemented"),
            self.token.clone(),
        ))
    }

    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String> {
        let arguments = self
            .arguments
            .iter()
            .map(|arg| (*arg).borrow().interpret(interpreter))
            .collect::<Result<Vec<Literal>, (String, Token)>>()
            .map_err(|(err, _)| err)?;

        for argument in &arguments {
            if let Literal::String(_) = argument {
                return Err(String::from("Cannot index array with string"));
            }
        }

        let key = format!(
            "{}({})",
            self.name,
            arguments
                .iter()
                .map(|arg| arg.clone().to_string())
                .reduce(|left, right| format!("{},{}", left, right))
                .ok_or(String::from("Cannot index array with empty selector ()"))?
        );

        interpreter.set(key, value);
        Ok(())
    }
}

#[derive(Debug)]
struct Unary {
    operator: Token,
    right: Rc<RefCell<dyn Expression>>,
}
impl Expression for Unary {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)> {
        match self.operator.symbol {
            Symbol::Plus => (*self.right).borrow().interpret(interpreter),
            Symbol::Minus => (*self.right)
                .borrow()
                .interpret(interpreter)?
                .negate()
                .ok_or((
                    String::from("Cannot negate non integer"),
                    self.operator.clone(),
                )),
            Symbol::Keyword(Keyword::Not) => {
                if let Literal::Number(num) = (*self.right).borrow().interpret(interpreter)? {
                    if num == 0.0 {
                        Ok(Literal::Number(1.0))
                    } else {
                        Ok(Literal::Number(0.0))
                    }
                } else {
                    Err((
                        String::from("Cannot negate non boolean"),
                        self.operator.clone(),
                    ))
                }
            }
            _ => Err((
                String::from("Could not perform unary operation"),
                self.operator.clone(),
            )),
        }
    }

    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String> {
        Err(String::from("Cannot assign unary"))
    }
}

#[derive(Debug)]
struct Binary {
    left: Rc<RefCell<dyn Expression>>,
    operator: Token,
    right: Rc<RefCell<dyn Expression>>,
}

fn plus(left: Literal, right: Literal) -> Result<Literal, (String, Token)> {
    if let Literal::Number(num) = left {
        if let Literal::Number(num2) = right {
            return Ok(Literal::Number(num + num2));
        }
    }

    Ok(Literal::String(format!(
        "{}{}",
        left.to_string(),
        right.to_string()
    )))
}

fn numeric_operation(
    left: Literal,
    right: Literal,
    op: fn(f32, f32) -> f32,
    err: String,
    token: Token,
) -> Result<Literal, (String, Token)> {
    if let Literal::Number(num) = left {
        if let Literal::Number(num2) = right {
            return Ok(Literal::Number(op(num, num2)));
        }
    }

    Err((err, token.clone()))
}

fn boolean_operation(
    left: Literal,
    right: Literal,
    op: fn(f32, f32) -> bool,
    err: String,
    token: Token,
) -> Result<Literal, (String, Token)> {
    if let Literal::Number(num) = left {
        if let Literal::Number(num2) = right {
            return Ok(Literal::Number(if op(num, num2) { 1.0 } else { 0.0 }));
        }
    }

    Err((err, token.clone()))
}

fn equality_operation(left: Literal, right: Literal) -> Literal {
    if let Literal::Number(num) = left {
        if let Literal::Number(num2) = right {
            return Literal::Number(if num == num2 { 1.0 } else { 0.0 });
        }
    }

    if let Literal::String(str) = left {
        if let Literal::String(str2) = right {
            return Literal::Number(if str == str2 { 1.0 } else { 0.0 });
        }
    }

    Literal::Number(0.0)
}

fn boolean_negation(literal: Literal) -> Literal {
    if let Literal::Number(num) = literal {
        Literal::Number(if num == 0.0 { 1.0 } else { 0.0 })
    } else {
        literal
    }
}

impl Expression for Binary {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<Literal, (String, Token)> {
        let left = (*self.left).borrow_mut().interpret(interpreter)?;
        let right = (*self.right).borrow_mut().interpret(interpreter)?;

        match self.operator.symbol.clone() {
            Symbol::Plus => plus(left, right),
            Symbol::Minus => numeric_operation(
                left,
                right,
                |l, r| l - r,
                String::from("Cannot subtract"),
                self.operator.clone(),
            ),
            Symbol::Slash => numeric_operation(
                left,
                right,
                |l, r| l / r,
                String::from("Cannot divide"),
                self.operator.clone(),
            ),
            Symbol::Star => numeric_operation(
                left,
                right,
                |l, r| l * r,
                String::from("Cannot multiply"),
                self.operator.clone(),
            ),
            Symbol::Carrot => numeric_operation(
                left,
                right,
                |l, r| l.powf(r),
                String::from("Cannot exponentiate"),
                self.operator.clone(),
            ),
            Symbol::Equals => Ok(equality_operation(left, right)),
            Symbol::NotEquals => Ok(boolean_negation(equality_operation(left, right))),
            Symbol::Greater => boolean_operation(
                left,
                right,
                |l, r| l > r,
                String::from("Cannot compare non number values"),
                self.operator.clone(),
            ),
            Symbol::GreaterEquals => boolean_operation(
                left,
                right,
                |l, r| l >= r,
                String::from("Cannot compare non number values"),
                self.operator.clone(),
            ),
            Symbol::Lesser => boolean_operation(
                left,
                right,
                |l, r| l < r,
                String::from("Cannot compare non number values"),
                self.operator.clone(),
            ),
            Symbol::LesserEquals => boolean_operation(
                left,
                right,
                |l, r| l <= r,
                String::from("Cannot compare non number values"),
                self.operator.clone(),
            ),
            _ => Err((
                String::from("Could not perform binary operation"),
                self.operator.clone(),
            )),
        }
    }

    fn assign(&self, interpreter: &mut Interpreter, value: Literal) -> Result<(), String> {
        Err(String::from("Cannot assign binary"))
    }
}

enum Operation {
    Goto(usize),
    End,
    Next,
}

trait Statement: Debug {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)>;
    fn get_label(&self) -> Option<usize>;
}

#[derive(Debug)]
struct Label {
    label: f32,
    statement: Rc<RefCell<dyn Statement>>,
}
impl Statement for Label {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        (*self.statement).borrow_mut().interpret(interpreter)
    }

    fn get_label(&self) -> Option<usize> {
        if self.label < 0.0 {
            None
        } else if self.label.trunc() != self.label {
            None
        } else {
            Some(self.label as usize)
        }
    }
}

#[derive(Debug)]
struct GoTo {
    token: Token,
    label: f32,
}
impl Statement for GoTo {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        if self.label < 0.0 {
            return Err((
                String::from("Cannot goto negative label"),
                self.token.clone(),
            ));
        }

        if self.label.trunc() != self.label {
            return Err((
                String::from("Cannot goto floating point label"),
                self.token.clone(),
            ));
        }

        Ok(Operation::Goto(self.label as usize))
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct End {}
impl Statement for End {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        Ok(Operation::End)
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct If {
    condition: Rc<RefCell<dyn Expression>>,
    body: Vec<Rc<RefCell<dyn Statement>>>,
}
impl Statement for If {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        if (*self.condition).borrow().interpret(interpreter)? == Literal::Number(0.0) {
            return Ok(Operation::Next);
        }

        for statement in self.body.clone() {
            if let Operation::Goto(label) = (*statement).borrow_mut().interpret(interpreter)? {
                return Ok(Operation::Goto(label));
            }
        }

        Ok(Operation::Next)
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct Assign {
    token: Token,
    target: Rc<RefCell<dyn Expression>>,
    value: Rc<RefCell<dyn Expression>>,
}
impl Statement for Assign {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        let value = self.value.borrow().interpret(interpreter)?;
        self.target
            .borrow()
            .assign(interpreter, value)
            .map_err(|message| (message, self.token.clone()))?;

        Ok(Operation::Next)
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct FunctionDefinition {
    name: String,
    arguments: Vec<String>,
    body: Rc<RefCell<dyn Expression>>,
}
impl Statement for FunctionDefinition {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        panic!("Function definition not implemented")
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct Print {
    token: Token,
    expressions: Vec<Rc<RefCell<dyn Expression>>>,
    spaced: Vec<bool>,
}
impl Statement for Print {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        if self.expressions.len() == 0 {
            println!("");
            return Ok(Operation::Next);
        }

        if self.expressions.len() != self.spaced.len() {
            return Err((
                String::from("Spacing doesn't match expressions"),
                self.token.clone(),
            ));
        }

        for (i, expression) in self.expressions.iter().enumerate() {
            if *self.spaced.get(i).unwrap() {
                print!("\t")
            }

            let value = expression.borrow().interpret(interpreter)?;
            print!("{}", value.to_string());
        }

        println!("");
        Ok(Operation::Next)
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

#[derive(Debug)]
struct Input {
    token: Token,
    prompt: Option<String>,
    variables: Vec<Rc<RefCell<dyn Expression>>>,
}
impl Statement for Input {
    fn interpret(&mut self, interpreter: &mut Interpreter) -> Result<Operation, (String, Token)> {
        if let Some(prompt) = self.prompt.clone() {
            print!("{}? ", prompt);
        }

        for variable in &self.variables {
            let input: String = read!();

            variable
                .as_ref()
                .borrow_mut()
                .assign(
                    interpreter,
                    match input.parse::<f32>() {
                        Ok(num) => Literal::Number(num),
                        Err(_) => Literal::String(input),
                    },
                )
                .map_err(|message| (message, self.token.clone()))?;
        }

        Ok(Operation::Next)
    }

    fn get_label(&self) -> Option<usize> {
        None
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn is_at_end(&mut self) -> bool {
        self.index >= self.tokens.len()
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn get(&self) -> Result<&Token, (String, Token)> {
        self.tokens.get(self.index).ok_or((
            String::from("Expected a token, but reached the end of the file"),
            self.tokens.last().unwrap().clone(),
        ))
    }

    fn previous(&self) -> Result<&Token, (String, Token)> {
        self.tokens.get(self.index - 1).ok_or((
            String::from("Expected a token, but reached way past the end of the file"),
            self.tokens.last().unwrap().clone(),
        ))
    }

    fn advance_previous(&mut self) -> Result<&Token, (String, Token)> {
        self.advance();
        self.previous()
    }

    fn expect(&mut self, symbol: Symbol) -> Result<&Token, (String, Token)> {
        let token = self.advance_previous()?;

        if token.symbol != symbol {
            Err((
                format!("Expected {:#?} but got {:#?}", symbol, token.symbol),
                token.clone(),
            ))
        } else {
            Ok(token)
        }
    }

    fn function_call(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let token = self.previous()?.clone();

        let function_name = if let Symbol::Identifier(function_name) = token.symbol.clone() {
            function_name
        } else {
            return Err((
                String::from("Expected identifier for function name in function call"),
                token.clone(),
            ));
        };

        self.expect(Symbol::LeftParen)?;

        let mut arguments: Vec<Rc<RefCell<dyn Expression>>> = Vec::new();

        let arg_token = self.get()?.clone();
        if arg_token.symbol != Symbol::RightParen {
            arguments.push(self.expression()?);

            while let Ok(arg_token) = self.get() {
                if arg_token.symbol != Symbol::Comma {
                    break;
                }

                self.expect(Symbol::Comma)?;
                arguments.push(self.expression()?)
            }
        }

        self.expect(Symbol::RightParen)?;
        Ok(Rc::from(RefCell::from(FunctionCall {
            name: function_name,
            token,
            arguments,
        })))
    }

    fn function_definition(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        self.expect(Symbol::Keyword(Keyword::Fn))?;

        let token = self.advance_previous()?;
        let function_name = if let Symbol::Identifier(function_name) = token.symbol.clone() {
            function_name
        } else {
            return Err((
                String::from("Expected identifier for function name in function definition"),
                token.clone(),
            ));
        };

        self.expect(Symbol::LeftParen)?;

        let mut arguments: Vec<String> = Vec::new();

        let token = self.get()?;
        if let Symbol::Identifier(name) = token.symbol.clone() {
            arguments.push(name);
            self.advance();

            while let Ok(token) = self.get() {
                if token.symbol != Symbol::Comma {
                    break;
                }

                let token = self.advance_previous()?;
                if let Symbol::Identifier(name) = token.symbol.clone() {
                    arguments.push(name);
                } else {
                    return Err((
                        String::from("Expected identifier following , in argument list in function definition"),
                        token.clone()
                    ));
                }
            }
        }

        self.expect(Symbol::RightParen)?;
        self.expect(Symbol::Equals)?;

        let body = self.expression()?;

        Ok(Rc::from(RefCell::from(FunctionDefinition {
            arguments,
            body,
            name: function_name,
        })))
    }

    fn base(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let token = self.get()?.clone();

        if token.symbol == Symbol::Keyword(Keyword::Fn) {
            self.advance();
            return self.function_call();
        }

        if let Symbol::Literal(literal) = token.symbol.clone() {
            self.advance();
            return Ok(Rc::from(RefCell::from(literal)));
        }

        self.var().map_err(|_| {
            (
                String::from("Could not recognise terminal token"),
                token.clone(),
            )
        })
    }

    fn unary(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let token = self.get()?;
        if token.symbol == Symbol::Plus || token.symbol == Symbol::Minus {
            let operator = token.clone();

            self.advance();
            Ok(Rc::from(RefCell::from(Unary {
                operator,
                right: self.not()?,
            })))
        } else {
            self.base()
        }
    }

    fn exponentiation(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.unary()?;

        while let Ok(token) = self.get() {
            if token.symbol != Symbol::Carrot {
                break;
            }

            let operator = token.clone();

            self.advance();
            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.unary()?,
            }))
        }

        Ok(expression)
    }

    fn production(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.exponentiation()?;

        while let Ok(token) = self.get() {
            if !(token.symbol == Symbol::Star || token.symbol == Symbol::Slash) {
                break;
            }

            let operator = token.clone();

            self.advance();
            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.exponentiation()?,
            }))
        }

        Ok(expression)
    }

    fn summation(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.production()?;

        while let Ok(token) = self.get() {
            if !(token.symbol == Symbol::Plus || token.symbol == Symbol::Minus) {
                break;
            }

            let operator = token.clone();

            self.advance();
            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.production()?,
            }))
        }

        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.summation()?;

        while let Ok(token) = self.get() {
            match token.symbol {
                Symbol::Equals | Symbol::NotEquals => (),
                Symbol::Greater | Symbol::Lesser => (),
                Symbol::GreaterEquals => (),
                Symbol::LesserEquals => (),
                _ => break,
            }

            let operator = token.clone();

            self.advance();
            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.summation()?,
            }))
        }

        Ok(expression)
    }

    fn not(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let token = self.get()?;
        if token.symbol == Symbol::Keyword(Keyword::Not) {
            let operator = token.clone();
            self.advance();

            Ok(Rc::from(RefCell::from(Unary {
                operator,
                right: self.not()?,
            })))
        } else {
            self.comparison()
        }
    }

    fn and(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.not()?;

        while let Ok(token) = self.get() {
            if token.symbol != Symbol::Keyword(Keyword::And) {
                break;
            }

            let operator = token.clone();
            self.advance();

            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.not()?,
            }))
        }

        Ok(expression)
    }

    fn or(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let mut expression = self.and()?;

        while let Ok(token) = self.get() {
            if token.symbol != Symbol::Keyword(Keyword::Or) {
                break;
            }

            let operator = token.clone();
            self.advance();

            expression = Rc::from(RefCell::from(Binary {
                left: expression,
                operator,
                right: self.and()?,
            }))
        }

        Ok(expression)
    }

    fn expression(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        self.or()
    }

    fn print(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let mut spaced = vec![false];

        let token = self.get()?.clone();

        let mut expressions: Vec<Rc<RefCell<dyn Expression>>> = Vec::new();
        if !vec![Symbol::Newline, Symbol::Colon].contains(&token.symbol) {
            expressions.push(self.expression()?);

            while let Some(token) = self.get().ok() {
                if !(token.symbol == Symbol::Comma || token.symbol == Symbol::SemiColon) {
                    break;
                }

                spaced.push(token.symbol == Symbol::SemiColon);
                self.advance();

                expressions.push(self.expression()?)
            }
        }

        Ok(Rc::from(RefCell::from(Print {
            token: token.clone(),
            expressions,
            spaced,
        })))
    }

    fn var(&mut self) -> Result<Rc<RefCell<dyn Expression>>, (String, Token)> {
        let token = self.get()?.clone();
        if let Symbol::Identifier(name) = token.symbol.clone() {
            self.advance();

            let next = self.get()?;
            if next.symbol == Symbol::LeftParen {
                return self.function_call();
            }

            return Ok(Rc::from(RefCell::from(Variable { name, token })));
        }

        Err((String::from("Expected variable identifier"), token))
    }

    fn let_(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let target = self.var()?;
        let next = self.advance_previous()?.clone();
        if next.symbol != Symbol::Equals {
            return Err((String::from("~~~"), next.clone()));
        }

        let value = self.expression()?;
        return Ok(Rc::from(RefCell::from(Assign {
            target,
            value,
            token: next.clone(),
        })));
    }

    fn goto(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let token = self.advance_previous()?;

        if let Symbol::Literal(Literal::Number(label)) = token.symbol {
            Ok(Rc::from(RefCell::from(GoTo {
                label,
                token: token.clone(),
            })))
        } else {
            Err((
                String::from("Expected integer/label after goto"),
                token.clone(),
            ))
        }
    }

    fn if_(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let condition = self.expression()?;

        self.expect(Symbol::Keyword(Keyword::Then))?;

        let next = self.get()?.clone();
        if let Symbol::Literal(Literal::Number(label)) = next.symbol {
            self.advance();
            return Ok(Rc::from(RefCell::from(If {
                condition,
                body: vec![Rc::from(RefCell::from(GoTo {
                    label,
                    token: next.clone(),
                }))],
            })));
        }

        let mut body = vec![self.operation()?];
        while let Ok(token) = self.get() {
            if token.symbol != Symbol::Colon {
                break;
            }
            self.advance();

            body.push(self.operation()?)
        }

        Ok(Rc::from(RefCell::from(If { condition, body })))
    }

    fn input(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let token = self.advance_previous()?.clone();

        let mut prompt = None;
        if let Symbol::Literal(Literal::String(prompt_string)) = token.symbol.clone() {
            prompt = Some(prompt_string);
            self.expect(Symbol::SemiColon)?;
        };

        let mut variables = vec![self.var()?];
        loop {
            let token = self.get()?.clone();
            if let Symbol::Comma = token.symbol.clone() {
                self.advance();
            } else {
                break;
            }

            if let Symbol::Identifier(_) = self.get()?.symbol.clone() {
                variables.push(self.var()?);
            } else {
                return Err((String::from("Expected variable identifier"), token));
            }
        }

        Ok(Rc::from(RefCell::from(Input {
            token: token,
            prompt,
            variables,
        })))
    }

    fn end(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        Ok(Rc::from(RefCell::from(End {})))
    }

    fn operation(&mut self) -> Result<Rc<RefCell<dyn Statement>>, (String, Token)> {
        let token = self.get()?.clone();

        let Symbol::Keyword(keyword) = (&token).symbol.clone() else {
            return self
                .let_()
                .map_err(|_| (String::from("Expected keyword"), token));
        };
        self.advance();

        let statement = match keyword {
            Keyword::Print => self.print(),
            Keyword::Def => self.function_definition(),
            Keyword::Let => self.let_(),
            Keyword::GoTo => self.goto(),
            Keyword::If => self.if_(),
            Keyword::Input => self.input(),
            Keyword::End => self.end(),
            _ => Err((String::from("Unrecognised keyword"), token)),
        }?;

        Ok(statement)
    }

    fn operations(&mut self) -> Result<Vec<Rc<RefCell<dyn Statement>>>, (String, Token)> {
        let mut operations = vec![self.operation()?];

        while let Ok(token) = self.get() {
            if token.symbol != Symbol::Colon {
                break;
            }
            self.advance();
            self.consume_whitespace()?;

            operations.push(self.operation()?)
        }

        self.expect(Symbol::Newline)?;
        Ok(operations)
    }

    fn consume_whitespace(&mut self) -> Result<(), (String, Token)> {
        let mut token = self.get()?;

        while let Symbol::Newline = token.symbol {
            self.advance();
            token = self.get()?;
        }

        Ok(())
    }

    fn label(&mut self) -> Result<Vec<Rc<RefCell<dyn Statement>>>, (String, Token)> {
        let token = self.get()?;

        if let Symbol::Literal(Literal::Number(label)) = token.symbol {
            self.advance();

            let mut rtn: Vec<Rc<RefCell<dyn Statement>>> = Vec::new();
            let mut is_first = true;
            let operations = self.operations()?;

            for operation in operations {
                if is_first {
                    is_first = false;
                    rtn.push(Rc::from(RefCell::from(Label {
                        label,
                        statement: operation,
                    })))
                } else {
                    rtn.push(operation)
                }
            }

            Ok(rtn)
        } else {
            self.operations()
        }
    }

    fn statements(&mut self) -> Result<Vec<Rc<RefCell<dyn Statement>>>, (String, Token)> {
        self.consume_whitespace()?;
        self.label()
    }

    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Rc<RefCell<dyn Statement>>>, (String, Token)> {
        let mut parser = Parser { tokens, index: 0 };
        let mut statements: Vec<Rc<RefCell<dyn Statement>>> = Vec::new();

        while !parser.is_at_end() {
            match parser.statements() {
                Ok(stmts) => {
                    for statement in stmts {
                        statements.push(statement);
                    }
                }
                Err((message, token)) => {
                    if token.symbol == Symbol::EOF {
                        return Ok(statements);
                    } else {
                        return Err((message, token));
                    }
                }
            }
        }

        Ok(statements)
    }
}

struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    variables: HashMap<String, Literal>,
}

impl Environment {
    pub fn from(parent: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            parent,
            variables: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: String, value: Literal) {
        self.variables.insert(key, value);
    }

    pub fn get(&mut self, key: String) -> Option<&Literal> {
        self.variables.get(&key)
    }

    pub fn print(&mut self) -> String {
        let parent = if let Some(parent) = self.parent.clone() {
            (*parent.clone()).borrow_mut().print()
        } else {
            String::from("null")
        };

        format!(
            "{{ \"parent\": {}, \"variables\": {:?} }}",
            parent, self.variables
        )
    }
}

struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    random: ThreadRng,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Rc::from(RefCell::from(Environment::from(None))),
            random: thread_rng(),
        }
    }

    pub fn push_environment(&mut self) -> Rc<RefCell<Environment>> {
        let rtn = self.environment.clone();

        self.environment = Rc::from(RefCell::from(Environment::from(Some(
            self.environment.clone(),
        ))));

        rtn
    }

    pub fn pop_environment(&mut self) {
        let parent = self.environment.borrow().parent.clone().unwrap();

        self.environment = parent;
    }

    pub fn set(&mut self, key: String, value: Literal) {
        (*self.environment).borrow_mut().set(key, value);
    }

    pub fn get(&mut self, key: String) -> Option<Literal> {
        let binding = self.environment.clone();
        let mut binding = (*binding).borrow_mut();
        let x = binding.get(key);
        x.map(|a| a.clone())
    }

    pub fn print(&mut self) -> String {
        (*self.environment).borrow_mut().print()
    }

    pub fn clone(&mut self) -> Interpreter {
        Interpreter {
            environment: self.environment.clone(),
            random: self.random.clone(),
        }
    }
}

#[cfg(test)]
mod environment_tests {
    use crate::{Interpreter, Literal};

    #[test]
    fn it_works() {
        let mut i = Interpreter::new();
        i.set(String::from("jeff"), Literal::String(String::from("hi")));

        let mut i2 = i.clone();

        i.push_environment();
        i.set(String::from("meff"), Literal::Number(10.0));

        i2.push_environment();
        i2.set(String::from("oof"), Literal::String(String::from("boof")));

        (&i2.environment.clone().borrow_mut())
            .parent
            .clone()
            .map(|parent| {
                (&mut parent.borrow_mut()).set(String::from("keif"), Literal::Number(420.0))
            });

        println!("i: {}", i.print());
        println!("i2: {}", i2.print());
    }
}

fn main() {
    let file = file_iterator();
    match tokenise(file) {
        Err((line, error, line_offset, character_offset)) => println!(
            "\nTokeniser Error@{}:{}:\n\t{}\n\tError: {}",
            line_offset + 1,
            character_offset,
            line,
            error
        ),
        Ok(tokens) => {
            fs::write(".tokens", format!("{:#?}", tokens)).unwrap();
            match Parser::parse(tokens) {
                Err((error, token)) => println!(
                    "\nParser Error@{}:{}:\n\t{} in \"{}\"\n\tError: {}",
                    token.line + 1,
                    token.char,
                    token.source,
                    file_iterator().nth(token.line).unwrap().unwrap(),
                    error
                ),
                Ok(statements) => {
                    let mut labels: HashMap<usize, usize> = HashMap::new();
                    let mut interpreter = Interpreter::new();

                    fs::write(".text", format!("{:#?}", statements)).unwrap();
                    for (i, statement) in (&statements).iter().enumerate() {
                        if let Some(label) = statement.borrow().get_label() {
                            labels.insert(label, i);
                        }
                    }

                    let mut instruction_pointer = 0;
                    loop {
                        let statement = statements.get(instruction_pointer).unwrap();

                        match statement
                            .clone()
                            .as_ref()
                            .borrow_mut()
                            .interpret(&mut interpreter)
                        {
                            Err((message, token)) => {
                                println!(
                                    "\nRuntime Error@{}:{}:\n\t{} in \"{}\"\n\tError: {}",
                                    token.line + 1,
                                    token.char,
                                    token.source,
                                    file_iterator().nth(token.line).unwrap().unwrap(),
                                    message
                                );
                                break;
                            }
                            Ok(operation) => match operation {
                                Operation::End => break,
                                Operation::Next => instruction_pointer += 1,
                                Operation::Goto(label) => {
                                    instruction_pointer = *labels.get(&label).unwrap()
                                }
                            },
                        }

                        // println!("{:#?}", statement)
                    }
                }
            }
        }
    }
}
