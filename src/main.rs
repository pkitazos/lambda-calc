use std::{collections::HashMap, fs::File, io::Read};
use stlc::{parse_term, typecheck};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    match read_code("examples/scratch.lc") {
        Some(code) => {
            println!("{:#?}", code);
            if let Ok((_, expr)) = parse_term(&code) {
                println!("{:#?}", expr);
                let env = HashMap::new();
                match typecheck(&env, &expr) {
                    Ok(t) => println!("Type: {:#?}", t),
                    Err(e) => println!("Type error: {}", e),
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
