use std::{error::Error, fs::File, io::Read};
use ulc::{interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    if let Err(e) = run("examples/scratch.lc") {
        eprintln!("{}", e);
    }
    Ok(())
}

fn run(filename: &str) -> Result<(), String> {
    let code = read_code(filename).ok_or("Failed to read file")?;

    println!("{}", code);

    let (_, program) =
        parser::parse(&code).map_err(|e| format!("Error parsing the code: \n{}", e))?;

    let env = interpreter::env_from_defs(&program.definitions)
        .map_err(|e| format!("Error building the initial env: \n{}", e))?;

    let expr = program.expression.ok_or("Nothing to do here")?;

    let result = interpreter::eval(expr, &env)
        .map_err(|e| format!("Error evaluating the program: \n{}", e))?;

    println!("\nResult: {}", result);
    Ok(())
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
