use anyhow::{Result, anyhow};
use crossterm::event;
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use std::{error::Error, fs::File, io::Read};
use ulc::{Show, Term, interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    if let Err(e) = run("untyped/examples/scratch.lc") {
        eprintln!("{}", e);
    }
    Ok(())
}

fn run(filename: &str) -> Result<()> {
    let code = read_code(filename).ok_or(anyhow!("Failed to read file"))?;

    let (_, program) =
        parser::parse(&code).map_err(|e| anyhow!("Error parsing the code: \n{}", e))?;

    let env = interpreter::env_from_defs(&program.definitions)
        .map_err(|e| anyhow!("Error building the initial env: \n{}", e))?;

    let expr = program.expression.ok_or(anyhow!("Nothing to do here"))?;

    // let result = interpreter::eval(expr, &env)
    //     .map_err(|e| format!("Error evaluating the program: \n{}", e))?;

    enable_raw_mode()?;
    println!("{}\r", expr);

    let result = event_loop(&expr);
    disable_raw_mode()?;
    result
}

fn event_loop(expr: &Term) -> Result<()> {
    let mut curr = expr.clone();
    let mut stack: Vec<Term> = vec![];

    loop {
        if let Some(key) = event::read()?.as_key_press_event() {
            match key.code {
                event::KeyCode::Left | event::KeyCode::Char('h') => {
                    if let Some(prev) = stack.pop() {
                        println!("{prev}\r");
                        curr = prev;
                    }
                }

                event::KeyCode::Right | event::KeyCode::Char('l') => {
                    if let Some(next) = interpreter::step(&curr) {
                        stack.push(curr.clone());
                        println!("{next}\r");
                        curr = next;
                    }
                }

                event::KeyCode::Esc | event::KeyCode::Char('q') => break,
                _ => {}
            }
        }
    }

    Ok(())
}

fn debug_print(prev: &Term, curr: &Term, stack: &[Term]) {
    println!("\nprev:\t{prev}\r");
    println!("curr:\t{curr}\r");
    let shown: Vec<Show> = stack.iter().map(Show).collect();
    let shown = format!("{shown:#?}").replace('\n', "\r\n");
    println!("stack:\t{shown}\r");
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
