use std::{error::Error, fs::File, io::Read};
use ulc::{interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    match read_code("examples/scratch.lc") {
        Some(code) => {
            println!("{}", code);
            match parser::parse(&code) {
                Ok((_, program)) => match interpreter::env_from_defs(&program.definitions) {
                    Ok(env) => match program.expression {
                        Some(expr) => match interpreter::eval(expr, &env) {
                            Ok(result) => println!("\nResult: {}", result),
                            Err(e) => eprintln!("{}", e),
                        },
                        None => {
                            println!("{:#?}", program);
                            eprintln!("erm")
                        }
                    },
                    Err(e) => eprintln!("{}", e),
                },

                Err(e) => eprintln!("{}", e),
            }
        }
        None => eprintln!("something went wrong"),
    }

    Ok(())
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
