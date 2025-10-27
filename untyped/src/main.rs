use std::{fs::File, io::Read};
use ulc::{interpreter, parser};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    match read_code("examples/scratch.lc") {
        Some(code) => {
            println!("{}", code);
            if let Ok((_, expr)) = parser::parse(&code) {
                let r = interpreter::eval(expr, &interpreter::empty_env());
                println!("\nResult: {}", r);
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
