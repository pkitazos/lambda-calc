use std::{error::Error, fs::File, io::Read};
use ulc::{interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    match read_code("examples/scratch.lc") {
        Some(code) => {
            println!("{}", code);
            if let Ok((_, program)) = parser::parse(&code) {
                let initial_env = interpreter::empty_env();
                // todo: build up initial env form program.definitions instead
                if let Some(expr) = program.expression {
                    match interpreter::eval(expr, &initial_env) {
                        Ok(result) => println!("\nResult: {}", result),
                        Err(e) => eprintln!("{}", e),
                    }
                }
            }
        }
        None => println!("something went wrong"),
    }

    Ok(())
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
