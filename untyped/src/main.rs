use std::{error::Error, fs::File, io::Read};
use ulc::{interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    match read_code("examples/scratch.lc") {
        None => eprintln!("Something went wrong reading the file"),

        Some(code) => {
            println!("{}", code);

            match parser::parse(&code) {
                Err(e) => eprintln!("Error parsing the code: \n{}", e),

                Ok((_, program)) => {
                    // println!("\n\nDefs: {:#?}", &program.definitions);

                    match interpreter::env_from_defs(&program.definitions) {
                        Err(e) => eprintln!("Error building the initial env: \n{}", e),

                        Ok(env) => {
                            // println!("\n\nEnv: {}", env);

                            match program.expression {
                                None => eprintln!("Nothing to do here"),

                                Some(expr) => match interpreter::eval(expr, &env) {
                                    Err(e) => eprintln!("Error evaluating the program: \n{}", e),
                                    Ok(result) => println!("\nResult: {}", result),
                                },
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
