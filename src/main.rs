use {
    loxide::{compile, execute},
    std::{
        env,
        fmt::Display,
        fs::File,
        io::{self, Read, Write},
        num::NonZeroI32,
        path::Path,
        process::exit,
    },
};

const EXIT_CODE_64: NonZeroI32 = unsafe { NonZeroI32::new_unchecked(64) };
const EXIT_CODE_65: NonZeroI32 = unsafe { NonZeroI32::new_unchecked(65) };
const EXIT_CODE_70: NonZeroI32 = unsafe { NonZeroI32::new_unchecked(70) };
const EXIT_CODE_74: NonZeroI32 = unsafe { NonZeroI32::new_unchecked(74) };

fn main() {
    exit(match run() {
        Ok(()) => 0,
        Err(exit_code) => exit_code.get(),
    });
}

fn run() -> Result<(), NonZeroI32> {
    let args = env::args().collect::<Vec<_>>();
    match args.len() {
        1 => run_repl(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: loxide [path]\n");
            Err(EXIT_CODE_64)
        }
    }
}

fn run_repl() -> Result<(), NonZeroI32> {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        let bytes_read = io::stdin().read_line(&mut line).map_err(|_| EXIT_CODE_74)?;
        if bytes_read == 0 {
            println!();
            break;
        }

        if let Ok(chunk) = compile(&line) {
            let _ = execute(chunk);
        }
    }

    Ok(())
}

fn run_file(path: impl AsRef<Path> + Display) -> Result<(), NonZeroI32> {
    let mut file = File::open(&path).map_err(|_| {
        eprintln!("Could not open file \"{}\".", path);
        EXIT_CODE_74
    })?;

    let mut source_code = String::new();
    file.read_to_string(&mut source_code).map_err(|_| {
        eprintln!("Could not read file \"{}\".", path);
        EXIT_CODE_74
    })?;

    let chunk = compile(&source_code).map_err(|_| EXIT_CODE_65)?;
    execute(chunk).map_err(|_| EXIT_CODE_70)?;

    Ok(())
}
