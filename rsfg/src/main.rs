extern crate docopt;
extern crate rsfg;
use docopt::Docopt;
use rsfg::{compile, CompileError};
use rvmfg::{call, Thread};
use std::path::Path;

const USAGE: &str = "
rsfg command line interface

Usage: rsfg <source> [<dest>]
       rsfg --run <source>
       rsfg --update-tests

Options:
    -r, --run       [DEBUG ONLY] Use rvmfg virtual machine to immediately run code
    --update-tests  [DEBUG ONLY] Spawn interactive prompt to update stdout of tests if correct
";

fn get_stdlib() -> String {
    let std_filename = "src/sfg/std.sfg";
    std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(filename: &Path) -> Result<Vec<u8>, CompileError> {
    let script_string = std::fs::read_to_string(filename).expect("could not load given file");
    let stdlib = get_stdlib();
    compile(&script_string, &stdlib)
}

fn main() {
    env_logger::init();
    let args = Docopt::new(USAGE).and_then(|d| d.parse()).unwrap_or_else(|e| e.exit());
    let script_filename = args.get_str("<source>");
    #[cfg(debug_assertions)]
    {
        if args.get_bool("--update-tests") {
            // TODO deal with needing <source>
            update_tests();
            std::process::exit(0);
        }
    }
    let script_path = Path::new(&script_filename);
    let result = compile_file(script_path);
    let compiled = match result {
        Ok(c) => c,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    if args.get_bool("--run") {
        let mut thread = Thread::new(compiled);
        call![thread.main()];
    } else {
        let out_path = args.get_str("<dest>");
        let out = if out_path == "" {
            script_path.with_extension("bcfg")
        } else {
            Path::new(&out_path).to_path_buf()
        };
        std::fs::write(out, compiled).expect("couldn't output compiled file");
    }
}

#[cfg(debug_assertions)]
fn update_tests() {
    // TODO: Actually write the tests this tests (and deduplicate)
    use std::io::*;
    for entry in std::fs::read_dir("tests/scripts/error").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension() == Some(&std::ffi::OsString::from("sfg")) {
            let pathstr = path.to_string_lossy();
            let out_path = path.with_extension("stderr");
            println!("COMPILING: {}", pathstr);
            let err = compile_file(&path).expect_err("compiled ok. fix?");
            let out = format!("{}", err);
            let mut old_str = String::new();
            let mut differs = true;
            if out_path.is_file() {
                let old = std::fs::read_to_string(out_path.clone()).unwrap();
                if old == out {
                    differs = false;
                } else {
                    old_str = format!("OLD STDERR:\n{}\n", old);
                }
            }
            println!("OUTPUT:\n{}", out);
            if differs {
                println!("{}save Y/n?", old_str);
            } else {
                println!("SAVED OUTPUT IS THE SAME. CONTINUING.");
                continue;
            }
            let do_write = loop {
                let mut input = String::new();
                stdin().read_line(&mut input).unwrap();
                match input.trim() {
                    "Y" | "y" | "" => break true,
                    "N" | "n" => break false,
                    _ => continue,
                };
            };
            if do_write {
                println!("SAVING.");
                let mut file = std::fs::File::create(out_path).unwrap();
                file.write_all(out.as_bytes()).unwrap();
            } else {
                println!("PRESERVING OLD.");
            }
        }
    }
    println!("NO MORE TESTS.");
}
